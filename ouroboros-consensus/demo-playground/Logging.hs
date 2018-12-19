{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logging (
    LogEvent -- opaque
  , logChain
  , showNetworkTraffic
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Semigroup ((<>))
import           Data.Text (pack)

import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Trace (logInfo)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock

import           Ouroboros.Consensus.Util.Condense

data LogEvent = LogEvent {
    msg    :: String
  }

showNetworkTraffic :: TBQueue LogEvent -> Trace IO -> IO ()
showNetworkTraffic q trace = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    logInfo trace $ pack msg

instance Condense BlockHeader where
    condense BlockHeader{..} =
      "{hash: " <> condense headerHash
                <> ", blockNo: "
                <> condense headerBlockNo
                <> "}"

logChain :: Condense [b]
         => TBQueue LogEvent
         -> Chain b
         -> IO ()
logChain loggingQueue chain = do
    let m = "Adopted chain: " <> condense (Chain.toOldestFirst chain)
    atomically $ writeTBQueue loggingQueue $ LogEvent m

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Condense BlockNo
deriving instance Condense ConcreteHeaderHash
