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

import Control.Exception.Base (throw)
import Data.ByteString.Lazy  as BS (ByteString)
import Codec.CBOR.Decoding (decodeWord, decodeListLenOf)
import Codec.CBOR.Read
import GHC.Stack (HasCallStack)

main :: IO ()
main = do
    putStrLn "main"

-- | Split the ByteString into the tag-word and the rest.
unwrapTopTerm :: ByteString -> (Word, ByteString)
unwrapTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs,tag) -> (tag,bs)
        Left err -> throw err

type MonoCodec x = Codec x Codec.CBOR.Read.DeserialiseFailure IO ByteString

decodeMsg :: HasCallStack => (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers "chainSyncParsers" chainSyncParsers
    1 -> error "reqRespMessage"
    2 -> error "pingPongMessage"
    3 -> error "blockFetchMessage"
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
  
      runCS = run (codecChainSync :: MonoCodec (CS.ChainSync () ()))
      
      chainSyncParsers = [
               runCS (ClientAgency TokIdle)
             , runCS (ServerAgency (TokNext TokCanAwait))
             , runCS (ClientAgency TokIdle)
             , runCS (ServerAgency TokIntersect)
             , runCS (ServerAgency TokIntersect)
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



