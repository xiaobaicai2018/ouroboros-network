{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main
where

import Ouroboros.Network.Protocol.ChainSync.Type as CS
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Network.TypedProtocol.ReqResp.Type as ReqResp
import Ouroboros.Network.Protocol.ReqResp.Codec (codecReqResp)
import Ouroboros.Network.Protocol.PingPong.Codec (codecPingPong)
import Network.TypedProtocol.PingPong.Type as PingPong
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import  Ouroboros.Network.Testing.ConcreteBlock 

import Network.TypedProtocol.Codec

import System.Process
import Control.Monad
import Control.Exception.Base (throw)
import Data.ByteString.Lazy  as BS (ByteString, readFile, hGetContents)
import qualified Codec.Serialise.Class as Serialise
import Codec.CBOR.Decoding (decodeWord, decodeListLenOf, decodeBytes)
import Codec.CBOR.Read
import GHC.Stack (HasCallStack)

main :: IO ()
main = replicateM_ 25 generateAndDecode 

generateAndDecode :: IO ()
generateAndDecode = do
    (_, Just hout, _, _) <- createProcess shellProcess
    BS.hGetContents hout >>= decodeMsg . decodeTopTerm
    where
       shellProcess = (shell "./cddl messages.cddl generate  | ./diag2cbor.rb -")
                             { std_out = CreatePipe }

decodeFile :: FilePath -> IO ()
decodeFile f =
    BS.readFile f >>= decodeMsg . decodeTopTerm

type MonoCodec x = Codec x Codec.CBOR.Read.DeserialiseFailure IO ByteString

data DummyBytes = DummyBytes
instance Serialise.Serialise DummyBytes where
    encode _ = error "encode Serialise DummyBytes"
    decode = decodeBytes >> return DummyBytes
type Body = DummyBytes

{-
data Header = Header
instance Serialise.Serialise Header where
    encode _ = error "encode Serialise Header"
    decode = decodeBytes >> return Header

instance HasHeader Header
instance StandardHash Header
-}

-- | Split the ByteString into the tag-word and the rest.
decodeTopTerm :: ByteString -> (Word, ByteString)
decodeTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs,tag) -> (tag,bs)
        Left err -> throw err

decodeMsg :: HasCallStack => (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers "chainSync"  chainSyncParsers
    1 -> tryParsers "reqResp"    reqRespParsers
    2 -> tryParsers "pingPong"   pingPongParsers
    3 -> tryParsers "blockFetch" blockFetchParsers
    4 -> error "txSubmissionMessage"
    5 -> error "muxControlMessage"
    _ -> error "unkown tag"
    where
        tryParsers :: String -> [IO Bool] -> IO ()
        tryParsers name [] = error $ "parse failed : " ++ name
        tryParsers name (h:t) = h >>= \case
            True -> return ()
            False -> tryParsers name t

        run :: forall ps (pr :: PeerRole) (st :: ps).
                Codec ps DeserialiseFailure IO ByteString
             -> PeerHasAgency pr st -> IO Bool
        run codec state = runCodec ((decode codec) state) input
  
        runCS = run (codecChainSync :: MonoCodec (ChainSync DummyBytes DummyBytes))
        chainSyncParsers = [
              runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency (CS.TokNext TokCanAwait))
            , runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency CS.TokIntersect)
            , runCS (ServerAgency CS.TokIntersect)
            ]

        runReqResp = run (codecReqResp :: MonoCodec (ReqResp DummyBytes DummyBytes))
        reqRespParsers = [
              runReqResp (ClientAgency ReqResp.TokIdle)
            , runReqResp (ServerAgency ReqResp.TokBusy) 
            ]

        runPingPong = run (codecPingPong :: MonoCodec PingPong)
        pingPongParsers = [
              runPingPong (ClientAgency PingPong.TokIdle)
            , runPingPong (ServerAgency PingPong.TokBusy) 
            ]

        runBlockFetch = run (codecBlockFetch :: MonoCodec (BlockFetch BlockHeader BlockBody))
        blockFetchParsers = [
              runBlockFetch (ClientAgency BlockFetch.TokIdle)
            , runBlockFetch (ServerAgency BlockFetch.TokBusy)
            , runBlockFetch (ServerAgency BlockFetch.TokStreaming)
            ]
        
runCodec
  :: IO (DecodeStep ByteString DeserialiseFailure IO (SomeMessage st))
  -> ByteString
  -> IO Bool
runCodec cont bs = cont >>= \case
    DecodeDone _msg _rest -> error "runCodec: codec is DecodeDone"
    DecodeFail _f -> error "runCodec: codec is DecodeFail"
    DecodePartial next -> (next $ Just bs) >>= \case
        DecodePartial _ -> return False
        DecodeDone _msg rest -> case rest of
            Nothing -> return True
            Just _r  -> return False
        DecodeFail _f -> return False
