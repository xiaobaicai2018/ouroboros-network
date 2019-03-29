{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Network.Mux.Control (
    -- * Versions
      Version (..)
    , Versioned (..)

    -- * Control Protocol
    , Control (..)
    , Message (..)
    , ClientHasAgency (..)
    , ServerHasAgency (..)

    -- ** Cbor codec

    -- ** Peers
    , runCtrlClient
    , runCtrlServer

    -- *** Inner interface, exposed for testing
    , clientCtrlPeer
    , serverCtrlPeer
    , runCtrlPeer

    -- ** Channel utils
    , muxBearerAsChannel
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word32)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Printf (printf)
import           GHC.Stack

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver (runDecoderWithChannel)

import           Ouroboros.Network.Codec ( toLazyByteString
                                         , convertCborDecoderLBS
                                         )
import           Ouroboros.Network.Mux.Types as Mx

-- |
-- Version data together with its encoder \/ decoder and continuation to run.
--
data Version m = forall versionData . Version
  { -- |
    -- Data associated with a given version.
    --
    versionData :: versionData

    -- |
    -- Encoder \/ decoder and continuation to run.
  , versioned :: Versioned m versionData
  }

-- |
-- Explicit encoder \/ decoder and a continuation for @'versionData'@.
-- It is responsibility of the continuation to check if the negotated @'versionData'@
-- agrees.
--
data Versioned m versionData = Versioned
  {
  -- |
  -- @versionData@ cbor encoder
    encoder :: versionData -> CBOR.Encoding

  -- |
  -- @versionData@ cbor decoder
  , decoder :: forall s . CBOR.Decoder s versionData

  -- |
  -- Continuation which runs mux with client's @versionData@.
  --
  , withVersionedData :: versionData -> m ()

  -- |
  -- Return @True@ if we can run the client's @versionData@.
  --
  -- TODO: provide a feedback, e.g.
  , agreeOnVersionedData :: versionData -> Bool
  }

runVersionData :: Version m -> m ()
runVersionData Version {versionData, versioned = Versioned {withVersionedData}}
    = withVersionedData versionData

agreeOnVersionData :: Version m -> Bool
agreeOnVersionData Version {versionData, versioned = Versioned {agreeOnVersionedData}}
    = agreeOnVersionedData versionData

-- |
-- State transitions of the version negotation protocol.
--
data Control m where
  StNegotate :: StNegotate -> Control m
  StAgree    :: StAgree    -> Control m
  StDone     :: Control m

data StNegotate where
  StInit    :: StNegotate
  StResp    :: StNegotate

data StAgree where
  StData    :: StAgree
  StConfirm :: StAgree

instance Protocol (Control m) where

  data Message (Control m) from to where
    -- |
    -- Send version which local node understands
    MsgCtrlReq :: [Word32] -> Message (Control m) ('StNegotate 'StInit) ('StNegotate 'StResp)

    -- |
    -- Receive a version which remote node accepted.  It should be one of the
    -- send versions, otherwise it is a protocol violation.
    MsgCtrlResp :: Word32  -> Message (Control m) ('StNegotate 'StResp) ('StAgree 'StData)

    -- |
    -- If a local node received a version that it does not understands, it
    -- sends back fail message and the negotiation will terminate.
    --
    MsgCtrlFail :: Text -> Message (Control m) ('StNegotate 'StResp) 'StDone

    -- |
    -- After negotation of version number send version data that the initiator
    -- wishes to use, e.g. this will include network magic number and versions
    -- of supported miniprotocols.
    --
    -- This messages sends back the negotated version.  This will allow the
    -- remote peer to decode @versionData@ included in @'Version' m@.  It must
    -- agree with what was received with @'MsgCtrlResp'@.
    --
    -- TODO: It may be possible to express this without the redundant @Word32@
    -- send back using 'typed-transitions' framework and its evolving codec.
    --
    MsgCtrlData :: Version m -> Message (Control m) ('StAgree 'StData) ('StAgree 'StConfirm)

    -- |
    -- After receiving version data, the remote end might agree or disagree to
    -- use it and the negotation will terminate.
    --
    MsgCtrlAgree    :: Message (Control m) ('StAgree 'StConfirm) 'StDone

    MsgCtrlDisagree :: Message (Control m) ('StAgree 'StConfirm) 'StDone 

  data ClientHasAgency st where
    TokInit :: ClientHasAgency ('StNegotate 'StInit)
    TokData :: ClientHasAgency ('StAgree 'StData)

  data ServerHasAgency st where
    TokResp    :: ServerHasAgency ('StNegotate StResp)
    TokConfirm :: ServerHasAgency ('StAgree 'StConfirm)

  data NobodyHasAgency st where
    TokDone    :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit tok = case tok of {}
  exclusionLemma_ClientAndServerHaveAgency TokData tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


encodeCtrlMsg :: forall (pr :: PeerRole) m st st'.
                 Message (Control m) st st'
              -> CBOR.Encoding
encodeCtrlMsg (MsgCtrlReq vs) =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 0
  <> CBOR.encodeListLen (fromIntegral $ length vs)
  <> foldMap CBOR.encodeWord32 vs
encodeCtrlMsg (MsgCtrlResp v) =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 1
  <> CBOR.encodeWord32 v
encodeCtrlMsg (MsgCtrlFail err) =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 2
  <> CBOR.encodeString err
encodeCtrlMsg (MsgCtrlData (Version {versionData, versioned})) =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 3
  <> encoder versioned versionData
encodeCtrlMsg MsgCtrlAgree =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 4
encodeCtrlMsg MsgCtrlDisagree =
     CBOR.encodeListLen 2
  <> CBOR.encodeWord 5


-- |
-- Decoding @'Message' ('Control' m) from to@ requirers a context.  After
-- agreeing of version number we will need the associated @'Version' m@.
--
data DecoderCtx (st :: Control m) where
  DecoderNegotateCtx :: DecoderCtx ('StNegotate a)
  DecoderAgreeCtx    :: Maybe (Version m) -> DecoderCtx ('StAgree a :: Control m)
  DecoderDoneCtx     :: DecoderCtx 'StDone

-- |
-- Context dependent decoder for @'Message' ('Control' m) from to@.
--
decodeCtrlMsg :: forall (pr :: PeerRole) s (st :: Control m).
                 DecoderCtx (st :: Control m)
              -> PeerHasAgency pr st
              -> CBOR.Decoder s (SomeMessage st)
decodeCtrlMsg ctx stok = do
  _ <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  case (stok, key, ctx) of
    (ClientAgency TokInit, 0, _) -> do
      l  <- CBOR.decodeListLen
      vs <- replicateM l CBOR.decodeWord32
      return $ SomeMessage $ MsgCtrlReq vs
    (ServerAgency TokResp, 1, _) -> SomeMessage . MsgCtrlResp <$> CBOR.decodeWord32
    (ServerAgency TokResp, 2, _) -> SomeMessage . MsgCtrlFail <$> CBOR.decodeString
    (ClientAgency TokData, 3, DecoderAgreeCtx (Just (Version {versioned}))) -> do
      _ <- CBOR.decodeListLen
      versionData <- decoder versioned
      return $ SomeMessage $ MsgCtrlData Version {versionData, versioned}
    -- TODO: better error message
    (ClientAgency TokData, 3, DecoderAgreeCtx Nothing) -> Fail.fail "codecControl: unknown context"
    (ServerAgency TokConfirm, 4, _) -> return $ SomeMessage MsgCtrlAgree
    (ServerAgency TokConfirm, 5, _) -> return $ SomeMessage MsgCtrlDisagree
    (_, _, _) -> Fail.fail ("codecControl: unknown tag " ++ show key)
              

-- |
-- Client @'Peer'@ of the version negotation protocol.
--
-- The client will throw if:
--
-- * received version number does not belong any of the versions which we sent
--   to the server,
-- * the server disagreed to run our version
--
--
clientCtrlPeer :: forall m.
                  MonadThrow m
               => Map Word32 (Version m)
               -> Peer (Control m) AsClient ('StNegotate 'StInit) m (m ())
clientCtrlPeer versions =
    -- send versions which we support
      Yield (ClientAgency TokInit) (MsgCtrlReq (Map.keys versions))
    -- await for a replay with a chosen version number
    $ Await (ServerAgency TokResp) $ \msg -> case msg of
        MsgCtrlFail e -> Effect $ throwM $ MuxError {
              errorType  = MuxControlNoMatchingVersion,
              errorMsg   = T.unpack e,
              errorStack = callStack
            }
        MsgCtrlResp v ->
          -- send version data
          case v `Map.lookup` versions of
            Nothing -> Effect $ throwM $ MuxError {
                  errorType  = MuxControlUnknownVersion,
                  errorMsg   = "not recognised version: " ++ show v,
                  errorStack = callStack
                }
            Just version ->
              -- send version to remote peer and await if it agrees on it
                Yield (ClientAgency TokData) (MsgCtrlData version)
              $ Await (ServerAgency TokConfirm) $ \msg -> case msg of
                  MsgCtrlAgree    -> Done TokDone (runVersionData version)
                  MsgCtrlDisagree ->
                    Effect $ throwM $ MuxError {
                        errorType  = MuxControlDisagreed,
                        errorMsg   = "remote peer disagreed to run proposed version " ++ show v,
                        errorStack = callStack
                      }

-- |
-- Server @'Peer'@ of the version negotation protocol.
--
-- The server will throw if:
--
-- * no matching version was found
-- * disagreed to run version received from the client
--
serverCtrlPeer :: forall m.
                  MonadThrow m
               => Map Word32 (Version m)
               -> Peer (Control m) AsServer ('StNegotate 'StInit) m (m ())
serverCtrlPeer versions = 
    -- await for incoming versions
    Await (ClientAgency TokInit) $ \(MsgCtrlReq vs) ->
      case Map.lookupMax $ Map.filterWithKey (\v _ -> v `elem` vs) versions of
        Nothing ->
          Yield (ServerAgency TokResp) (MsgCtrlFail $ T.pack "no matching version")
            $ Effect $ throwM $ MuxError {
                  errorType  = MuxControlNoMatchingVersion,
                  errorMsg   = show vs,
                  errorStack = callStack
                }
        Just (v, _) ->
            Yield (ServerAgency TokResp) (MsgCtrlResp v)
          $ Await (ClientAgency TokData) $ \(MsgCtrlData version1) ->
              case agreeOnVersionData version1 of
                True  -> Yield (ServerAgency TokConfirm) MsgCtrlAgree
                       $ Done TokDone (runVersionData version1)
                False -> Yield (ServerAgency TokConfirm) MsgCtrlDisagree
                       $ Effect $ throwM $ MuxError {
                           errorType  = MuxControlDisagreed,
                           errorMsg   = "local peer disagreed to run proposed version " ++ show v,
                           errorStack = callStack
                         }

-- |
-- Like @'runPeer'@.  It will evolve @'DecoderCtx'@ and use it to run
-- @'decodeCtrlMsg'@ on incomming data.
--
runCtrlPeer
  :: forall ps (st :: ps) pr bytes m a .
     ( MonadThrow m
     , MonadST m
     )
  => Map Word32 (Version m)
  -> Channel m ByteString
  -> Peer (Control m) pr ('StNegotate 'StInit) m a
  -> m a
runCtrlPeer versions channel@Channel{send} =
    go Nothing DecoderNegotateCtx
  where
    go :: forall st'.
          Maybe ByteString
       -> DecoderCtx st'
       -> Peer (Control m) pr st' m a
       -> m a
    go trailing ctx (Effect k) = k >>= go trailing ctx

    go _        _ (Done _ x) = return x

    go trailing ctx (Yield stok msg k) = do
      send (toLazyByteString $ CBOR.toBuilder $ encodeCtrlMsg msg)
      go trailing (nextCtx ctx msg) k

    go trailing ctx (Await stok k) = do
      decoder <- convertCborDecoder (decodeCtrlMsg ctx stok)
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (nextCtx ctx msg) (k msg)
        Left failure                       -> throwM failure
    
    nextCtx :: forall st' st''.
               DecoderCtx st'
            -> Message (Control m) st' st''
            -> DecoderCtx st''
    nextCtx _                     MsgCtrlReq{}    = DecoderNegotateCtx
    nextCtx _                     (MsgCtrlResp v) = DecoderAgreeCtx (v `Map.lookup` versions)
    nextCtx _                     MsgCtrlFail{}   = DecoderDoneCtx
    nextCtx (DecoderAgreeCtx ctx) MsgCtrlData{}   = DecoderAgreeCtx ctx
    nextCtx _                     MsgCtrlAgree{}  = DecoderDoneCtx
    nextCtx _                     MsgCtrlAgree{}  = DecoderDoneCtx

    convertCborDecoder
      :: forall x st0.
        (forall s. CBOR.Decoder s (SomeMessage st0))
      -> m (DecodeStep ByteString CBOR.DeserialiseFailure m (SomeMessage st0))
    convertCborDecoder cborDecode =
        withLiftST (convertCborDecoderLBS cborDecode)


-- |
-- Run client side of the version negotiation; after successful negotiation phase
-- start the inner computation specified by chosen @'Version'@.
--
runCtrlClient
  :: forall m ptcl.
     ( MonadThrow m
     , MonadST m
     , Eq ptcl
     , Show ptcl
     )
  => Map Word32 (Version m)
  -> MuxBearer ptcl m
  -> m ()
runCtrlClient versions bearer = 
  let channel = muxBearerAsChannel bearer ModeInitiator
  in join $ runCtrlPeer versions channel (clientCtrlPeer versions)


-- |
-- Run server side of the version negotiation; after successful negotiation phase
-- start the inner computation specified by chosen @'Version'@.
--
runCtrlServer
  :: forall m ptcl.
     ( MonadThrow m
     , MonadST m
     , Eq ptcl
     , Show ptcl
     )
  => Map Word32 (Version m)
  -> MuxBearer ptcl m
  -> m ()
runCtrlServer versions bearer = 
  let channel = muxBearerAsChannel bearer ModeInitiator
  in join $ runCtrlPeer versions channel (serverCtrlPeer versions)

-- |
-- Interpret @'MuxBearer'@ as a @'Channel'@ for the version negotation
-- protocol.
--
muxBearerAsChannel
  :: ( MonadThrow m
     , Eq ptcl
     , Show ptcl
     )
  => MuxBearer ptcl m
  -> MiniProtocolMode
  -> Channel m ByteString
muxBearerAsChannel bearer mode = Channel {send, recv}
    where
      send bs  = void $ Mx.write bearer (MuxSDU (RemoteClockModel 0) Muxcontrol mode (fromIntegral $ BL.length bs) bs)

      recv = do
        (MuxSDU {msBlob, msMode, msId} , _) <- Mx.read bearer
        when (msId /= Muxcontrol || msMode == mode)
          $ throwM $ MuxError {
              errorType  = MuxUnknownMiniProtocol,
              errorMsg   = printf "invalid id or mode: %s %s" (show msId) (show msMode),
              errorStack = callStack
            }
        return (Just msBlob)
