{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Consensus.Node (
    -- * Node IDs
    NodeId(..)
  , CoreNodeId(..)
  , fromCoreNodeId
    -- * Node
  , NodeKernel(..)
  , NodeCallbacks(..)
  , NodeComms(..)
  , nodeKernel
    -- * Channels (re-exports from the network layer)
  , Channel
  , Network.createConnectedChannels
  , Network.loggingChannel
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad
import           Crypto.Random (ChaChaDRG)
import           Data.Map.Strict (Map)
import           Data.Typeable (Typeable)
import           Data.Void (Void, vacuous)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel as Network
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
                     (BlockFetchConsensusInterface (..), blockFetchLogic,
                     newFetchClientRegistry)
import           Ouroboros.Network.BlockFetch.State (FetchMode)
import           Ouroboros.Network.BlockFetch.Types (SizeInBytes)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment (..), Point)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB


{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

-- TODO: This was moved here from the network layer Node module. We should
-- review this and make sure it makes sense here.
data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show)

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i

-- | Core node ID
newtype CoreNodeId = CoreNodeId Int
  deriving (Show, Eq, Ord, Condense, Serialise)

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId (CoreNodeId n) = CoreId n

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m up blk hdr = NodeKernel {
      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      addUpstream       :: forall e bytes. Exception e
                        => up -> NodeComms e m hdr bytes -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addDownstream     :: forall e bytes. Exception e
                        => NodeComms e m hdr bytes -> m ()
    }

-- | Monad that we run protocol specific functions in
type ProtocolM blk m = NodeStateT (BlockProtocol blk) (ChaChaT (STM m))

-- | Callbacks required when initializing the node
data NodeCallbacks m blk = NodeCallbacks {
      -- | Produce a block
      produceBlock :: IsLeader (BlockProtocol blk) -- ^ Proof we are leader
                   -> ExtLedgerState blk -- ^ Current ledger state
                   -> SlotNo             -- ^ Current slot
                   -> Point blk          -- ^ Previous point
                   -> BlockNo            -- ^ Previous block number
                   -> ProtocolM blk m blk

      -- | Produce a random seed
      --
      -- We want to be able to use real (crypto strength) random numbers, but
      -- obviously have no access to a sytem random number source inside an
      -- STM transaction. So we use the system RNG to generate a local DRG,
      -- which we then use for this transaction, and /only/ this transaction.
      -- The loss of entropy therefore is minimal.
      --
      -- In IO, can use 'Crypto.Random.drgNew'.
    , produceDRG :: m ChaChaDRG
    }

nodeKernel :: forall m blk hdr up.
              ( MonadSTM m
              , MonadCatch (STM m)
              , MonadFork m
              , MonadThrow m
              , ProtocolLedgerView blk
              , Eq hdr
              , HasHeader hdr
              , HeaderHash hdr ~ HeaderHash blk
              , BlockProtocol hdr ~ BlockProtocol blk
              , Ord up
              , Typeable hdr
              , Typeable blk
              )
           => NodeConfig (BlockProtocol blk)
           -> NodeState (BlockProtocol blk)
           -> BlockchainTime m
           -> ChainDB m blk hdr (ExtLedgerState blk)
           -> NodeCallbacks m blk
           -> m (NodeKernel m up blk hdr)
nodeKernel cfg initState btime chainDB callbacks = do
    st <- initInternalState cfg initState btime chainDB callbacks

    forkBlockProduction  st

    return NodeKernel {
        addUpstream       = npAddUpstream   (networkLayer st)
      , addDownstream     = npAddDownstream (networkLayer st)
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

data InternalState m up blk hdr = IS {
      cfg           :: NodeConfig (BlockProtocol blk)
    , btime         :: BlockchainTime m
    , callbacks     :: NodeCallbacks m blk
    , networkLayer  :: NetworkProvides m up hdr
    , chainDB       :: ChainDB m blk hdr (ExtLedgerState blk)
                       -- ^ TODO ExtLedgerState correct here?
    , varCandidates :: TVar m (Candidates up hdr)
    , varState      :: TVar m (NodeState (BlockProtocol blk))
    }

initInternalState :: forall m up blk hdr.
                     ( MonadSTM m
                     , MonadCatch (STM m)
                     , MonadFork m
                     , MonadThrow m
                     , HasHeader hdr
                     , HeaderHash hdr ~ HeaderHash blk
                     , ProtocolLedgerView blk
                     , BlockProtocol hdr ~ BlockProtocol blk
                     , Eq hdr
                     , Ord up
                     , Typeable hdr
                     , Typeable blk
                     )
                  => NodeConfig (BlockProtocol blk) -- ^ Node configuration
                  -> NodeState (BlockProtocol blk)  -- ^ Init node state
                  -> BlockchainTime m               -- ^ Time
                  -> ChainDB m blk hdr (ExtLedgerState blk)
                  -> NodeCallbacks m blk            -- ^ Callbacks
                  -> m (InternalState m up blk hdr)
initInternalState cfg initState btime chainDB callbacks = do
    varCandidates <- atomically $ newTVar noCandidates
    varState      <- atomically $ newTVar initState

    blockFetch    <- initBlockFetchConsensusInterface cfg chainDB varCandidates

    let networkRequires :: NetworkRequires m up blk hdr
        networkRequires = NetworkRequires {
            nrBlockFetch = blockFetch
          , nrSyncClient = consensusSyncClient
                             cfg
                             btime
                             (ClockSkew 1) -- TODO make a parameter
                             chainDB
                             varCandidates
          }

    networkLayer <- initNetworkLayer networkRequires

    return IS{..}

initBlockFetchConsensusInterface
    :: forall m peer block header.
       ( MonadSTM m
       , OuroborosTag (BlockProtocol block)
       , HasHeader header
       , Eq header
       )
    => NodeConfig (BlockProtocol block)
    -> ChainDB m block header (ExtLedgerState block)
    -> TVar m (Candidates peer header)
    -> m (BlockFetchConsensusInterface peer header block m)
initBlockFetchConsensusInterface cfg chainDB varCandidates =
    return BlockFetchConsensusInterface {..}
  where
    readCandidateChains :: STM m (Map peer (ChainFragment header))
    readCandidateChains = candidates <$> readTVar varCandidates

    readCurrentChain :: STM m (ChainFragment header)
    readCurrentChain = ChainDB.getCurrentChain chainDB

    readFetchMode :: STM m FetchMode
    readFetchMode = error "TODO"

    readFetchedBlocks :: STM m (Point block -> Bool)
    readFetchedBlocks = ChainDB.getIsFetched chainDB

    addFetchedBlock :: Point block -> block -> m ()
    addFetchedBlock = ChainDB.addBlock chainDB

    plausibleCandidateChain :: ChainFragment header
                            -> ChainFragment header
                            -> Bool
    plausibleCandidateChain = preferCandidate cfg

    compareCandidateChains :: ChainFragment header
                           -> ChainFragment header
                           -> Ordering
    compareCandidateChains = compareCandidates cfg

    blockFetchSize :: header -> SizeInBytes
    blockFetchSize = error "TODO"

    blockMatchesHeader :: header -> block -> Bool
    blockMatchesHeader = error "TODO"

forkBlockProduction :: forall m up blk hdr.
                       ( MonadSTM m
                       , ProtocolLedgerView blk
                       , HasHeader hdr
                       , HeaderHash hdr ~ HeaderHash blk
                       )
                    => InternalState m up blk hdr -> m ()
forkBlockProduction IS{..} =
    onSlotChange btime $ \currentSlot -> do
      drg  <- produceDRG
      mNewBlock <- atomically $ do
        varDRG <- newTVar drg
        l@ExtLedgerState{..} <- ChainDB.getCurrentLedger chainDB
        mIsLeader            <- runProtocol varDRG $
                                   checkIsLeader
                                     cfg
                                     currentSlot
                                     (protocolLedgerView cfg ledgerState)
                                     ouroborosChainState

        case mIsLeader of
          Nothing    -> return Nothing
          Just proof -> do
            (prevPoint, prevNo) <- prevPointAndBlockNo currentSlot <$>
              ChainDB.getCurrentChain chainDB
            newBlock <- runProtocol varDRG $
              produceBlock proof l currentSlot (castPoint prevPoint) prevNo
            return $ Just newBlock

      whenJust mNewBlock $ \newBlock ->
        ChainDB.addBlock chainDB (blockPoint newBlock) newBlock
  where
    NodeCallbacks{..} = callbacks

    -- Return the point and block number of the most recent block in the
    -- current chain with a slot < the given slot. These will either
    -- correspond to the block at the tip of the current chain or, in case
    -- another node was also elected leader and managed to produce a block
    -- before us, the block right before the one at the tip of the chain.
    prevPointAndBlockNo :: SlotNo -> ChainFragment hdr -> (Point hdr, BlockNo)
    prevPointAndBlockNo slot c = case c of
        Empty -> (Chain.genesisPoint, Chain.genesisBlockNo)
        c' :> b -> case blockSlot b `compare` slot of
          LT -> (blockPoint b, blockNo b)
          GT -> error "prevPointAndBlockNo: block in future"
          -- The block at the tip has the same slot as the block we're going
          -- to produce (@slot@), so look at the block before it.
          EQ | _ :> b' <- c'
             -> (blockPoint b', blockNo b')
             | otherwise
               -- If there is no block before it, use genesis.
             -> (Chain.genesisPoint, Chain.genesisBlockNo)

    runProtocol :: TVar m ChaChaDRG -> ProtocolM blk m a -> STM m a
    runProtocol varDRG = simOuroborosStateT varState
                       $ simChaChaT varDRG
                       $ id

{-------------------------------------------------------------------------------
  New network layer
-------------------------------------------------------------------------------}

data NetworkRequires m up blk hdr = NetworkRequires {
      -- | Start tracking a new upstream node
      --
      -- Although it is (typically) the responsibility of the network layer to
      -- decide whether or not to track another peer, each time it does decide
      -- to do so, it will ask the consensus layer for a client to track this
      -- upstream node. It will be the responsibility of this client to do
      -- block validation and implement the logic required to implement the
      -- genesis rule.
      nrSyncClient :: up -> Consensus ChainSyncClient hdr m

      -- | The consensus layer functionality that the block fetch logic
      -- requires.
    , nrBlockFetch :: BlockFetchConsensusInterface up hdr blk m
    }

-- | Required by the network layer to initiate comms to a new node
data NodeComms e m hdr bytes = NodeComms {
      -- | Codec for concrete send type @s@ and receive type @r@
      ncCodec    :: Codec (ChainSync hdr (Point hdr)) e m bytes
      -- TODO: will need to handle these 'e' exceptions properly

      -- | Construct a channel to the node
      --
      -- This is in CPS style to allow for resource allocation. However, it
      -- is important to note that this resource allocation will run in a thread
      -- which itself is untracked, so if resource deallocation absolutely
      -- /must/ happen additional measures must be taken
    , ncWithChan :: forall a. (Channel m bytes -> m a) -> m a
    }

data NetworkProvides m up hdr = NetworkProvides {
      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      npAddUpstream   :: forall e bytes. Exception e
                      => up -> NodeComms e m hdr bytes -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , npAddDownstream :: forall e bytes. Exception e
                      => NodeComms e m hdr bytes -> m ()
    }

initNetworkLayer :: forall m up hdr blk.
                    ( MonadSTM m
                    , MonadFork m
                    , MonadThrow m
                    , HasHeader hdr
                    , Ord up
                    , HasHeader blk
                    , HeaderHash hdr ~ HeaderHash blk
                    )
                 => NetworkRequires m up blk hdr
                 -> m (NetworkProvides m up hdr)
initNetworkLayer NetworkRequires{..} = do
    -- The chain producer state is entirely the responsibility of the network
    -- layer; it does not get exposed in the 'NetworkLayer' API. Moreover, it
    -- is not necessary for the chain in the chain producer state to be updated
    -- synchronously with our chain, it is fine for this to lag. TODO update
    cpsVar <- atomically $ do
      chain <- readCurrentChain nrBlockFetch
      -- TODO use ChainFragment in ChainProducerState
      newTVar $ initChainProducerState (CF.toChain chain)

    -- We also continously monitor our own chain, so that we can update our
    -- downstream peers when our chain changes
    void $ fork $ forever $ atomically $ do
      chain <- readCurrentChain nrBlockFetch
      cps   <- readTVar cpsVar
      -- TODO: We should probably not just compare the slot
      if CF.headOrGenPoint chain == Chain.headPoint (chainState cps)
        then retry
             -- TODO let switchFork take a ChainFragment
        else modifyTVar cpsVar (switchFork (CF.toChain chain))

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    fetchClientRegistry <- newFetchClientRegistry
    runInBg $ blockFetchLogic nullTracer nrBlockFetch fetchClientRegistry

    return $ NetworkProvides {
          npAddDownstream = \NodeComms{..} -> do
            -- TODO use an iterator from the ChainDB in chainSyncServerPeer
            -- instead of ChainProducerState
            --
            -- TODO server side of chain sync protocol will be instantiated to
            -- the product of a block header + block size
            --
            -- TODO update chainSyncServerExample
            let producer = chainSyncServerPeer (chainSyncServerExample () cpsVar)
            void $ fork $ void $ ncWithChan $ \chan ->
              runPeer nullTracer ncCodec chan producer
              --TODO: deal with the exceptions this throws, use async
        , npAddUpstream = \up NodeComms{..} -> do
            let consumer = nrSyncClient up
            void $ fork $ void $ ncWithChan $ \chan ->
              runPeer nullTracer ncCodec chan (chainSyncClientPeer consumer)
              --TODO: deal with the exceptions this throws, use async
        }
  where
    --  TODO use async
    runInBg :: m Void -> m ()
    runInBg = void . fork . void . vacuous
