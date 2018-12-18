{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run (
      runNode
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, atomically, newTBQueue,
                     newTVar, readTVar, writeTVar)
import           Control.Monad
import           Control.Monad.ST (stToIO)
import           Control.Monad.Trans
import           Crypto.Random
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.Set (fromList)
import           Data.Text (pack)

import           Protocol.Codec (hoistCodec)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Observable (ObservableInstance (..))
import           Cardano.BM.Data.SubTrace (SubTrace (..))
import           Cardano.BM.Data.Trace (Trace, TraceContext (..))
import           Cardano.BM.Observer.Monadic (bracketObserveM)
import           Cardano.BM.Output.Switchboard (unrealize)
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (appendName, logNotice)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (pointHash)
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

import           CLI
import           Logging
import           Mock.Mempool (Mempool, collect, mempoolRemove)
import           Mock.TxSubmission
import           NamedPipe (DataFlow (..), NodeMapping ((:==>:)))
import qualified NamedPipe
import           Topology

runNode :: CLI -> IO ()
runNode cli@CLI{..} = do
    trace@(ctx, _) <- setupTrace (Left configFile) "demo-playground"
    CM.setSubTrace (configuration ctx) "demo-playground.submit-tx" $
        Just $ ObservableTrace observablesSet
    -- different names for seperate functionalities
    submitTxTrace <- subTrace   "submit-tx"   trace
    nodeTrace     <- appendName "simple-node" trace
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter topology tx ->
           handleTxSubmission submitTxTrace topology tx
         SimpleNode topology ->
            case protocol of
              Some p -> case demoProtocolConstraints p of
                           Dict -> handleSimpleNode nodeTrace p cli topology

    threadDelay 1000
    logNotice trace "exiting..."
    -- close scribes (finalizer) needed to flush the queues.
    unrealize $ switchboard ctx
  where
    observablesSet = fromList [MonotonicClock, MemoryStats]

-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: forall p. DemoProtocolConstraints p
                 => Trace IO -> DemoProtocol p -> CLI -> TopologyInfo -> IO ()
handleSimpleNode trace0@(ctx, _) p CLI{..} (TopologyInfo myNodeId topologyFile) = do
    let nodeIdText = pack . show $ myNodeId
    CM.setSubTrace
        (configuration ctx)
        ("demo-playground.simple-node." <> nodeIdText <> ".forgeBlock")
        (Just $ ObservableTrace $ fromList [MonotonicClock])

    trace <- appendName nodeIdText trace0

    logNotice trace $ "System started at " <> pack (show systemStart)
    topoE <- readTopologyFile topologyFile
    case topoE of
         Left e -> error e
         Right t@(NetworkTopology nodeSetups) -> do
             let topology  = toNetworkMap t
                 nodeSetup = fromMaybe (error "node not found.") $
                                   M.lookup myNodeId topology

             logNotice trace $ "**************************************"
             logNotice trace $ "I am Node = " <> pack (show myNodeId)
             logNotice trace $ "My consumers are " <> pack (show (consumers nodeSetup))
             logNotice trace $ "My producers are " <> pack (show (producers nodeSetup))
             logNotice trace $ "**************************************"

             let ProtocolInfo{..} = protocolInfo
                                      p
                                      (NumCoreNodes (length nodeSetups))
                                      (CoreNodeId nid)

             -- Creates a TBQueue to be used by all the logger threads to monitor
             -- the traffic.
             loggingQueue    <- atomically $ newTBQueue 50
             terminalTrace <- appendName "terminal-logger" trace
             terminalThread  <- spawnTerminalLogger loggingQueue terminalTrace

             let initialPool :: Mempool Mock.Tx
                 initialPool = mempty

             -- Each node has a mempool, regardless from its consumer
             -- and producer threads.
             nodeMempool <- atomically $ newTVar initialPool

             let callbacks :: NodeCallbacks IO (MonadPseudoRandomT ChaChaDRG) (Block p)
                 callbacks = NodeCallbacks {
                     produceBlock = \proof l slot prevPoint prevBlockNo -> do
                        let curNo    = succ prevBlockNo
                            prevHash = castHash (pointHash prevPoint)

                        -- Before generating a new block, look for incoming transactions.
                        -- If there are, check if the mempool is consistent and, if it is,
                        -- grab the valid new transactions and incorporate them into a
                        -- new block.
                        mp  <- lift . lift $ readTVar nodeMempool
                        txs <- do let ts  = collect (Mock.slsUtxo . ledgerState $ l) mp
                                      mp' = mempoolRemove (M.keysSet ts) $ mp
                                  lift . lift $ writeTVar nodeMempool mp'
                                  return ts

                        bracketObserveM trace "forgeBlock" Mock.forgeBlock pInfoConfig
                                        slot
                                        curNo
                                        prevHash
                                        txs
                                        proof
                 , adoptedNewChain = logChain loggingQueue
                 }

             -- TODO: This use of STM is actually not correct, we need to revisit
             -- this one and use a SystemDRG (which lives in IO).
             randomnessSource <- atomically $ newTVar (seedToChaCha nullSeed)
             blockchainTime <- realBlockchainTime systemStart slotDuration


             kernelHandle <-
                 nodeKernel pInfoConfig
                            pInfoInitState
                            (simMonadPseudoRandomT randomnessSource)
                            blockchainTime
                            pInfoInitLedger
                            pInfoInitChain
                            callbacks

             -- Spawn the thread which listens to the mempool.
             mempoolThread <-
                     spawnMempoolListener myNodeId nodeMempool kernelHandle


             forM_ (producers nodeSetup) (addUpstream kernelHandle)
             forM_ (consumers nodeSetup) (addDownstream kernelHandle)

             let allThreads = terminalThread : [mempoolThread]
             void $ Async.waitAnyCancel allThreads

  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      spawnTerminalLogger :: TBQueue LogEvent -> Trace IO -> IO (Async.Async ())
      spawnTerminalLogger q tr = do
          Async.async $ showNetworkTraffic q tr

      -- We need to make sure that both nodes read from the same file
      -- We therefore use the convention to distinguish between
      -- upstream and downstream from the perspective of the "lower numbered" node
      addUpstream :: NodeKernel IO NodeId (Block p)
                  -> NodeId
                  -> IO ()
      addUpstream kernel producerNodeId = do
        let direction = Upstream (producerNodeId :==>: myNodeId)
        registerUpstream (nodeNetworkLayer kernel)
                         producerNodeId
                         (hoistCodec stToIO codecChainSync) $ \cc ->
          NamedPipe.withPipe direction $ \(hndRead, hndWrite) ->
            cc (P.pipeDuplex hndRead hndWrite)

      addDownstream :: NodeKernel IO NodeId (Block p)
                    -> NodeId
                    -> IO ()
      addDownstream kernel consumerNodeId = do
        let direction = Downstream (myNodeId :==>: consumerNodeId)
        registerDownstream (nodeNetworkLayer kernel)
                           (hoistCodec stToIO codecChainSync)
                           $ \cc ->
          NamedPipe.withPipe direction $ \(hndRead, hndWrite) -> do
              cc (P.pipeDuplex hndRead hndWrite)
