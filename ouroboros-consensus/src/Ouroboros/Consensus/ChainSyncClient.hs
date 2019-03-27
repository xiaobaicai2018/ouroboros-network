{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncClient (
    Consensus
  , consensusSyncClient
  , ConsensusClientException
  , ClockSkew (..)

    -- * Candidates
  , Candidates
  , candidates
  , noCandidates
  , updateCandidates
  ) where


import           Control.Monad
import           Control.Monad.Except
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block
import           Ouroboros.Network.ChainFragment (ChainFragment (..), Point)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB


{-------------------------------------------------------------------------------
  (Consensus layer provided) Chain sync client

  TODO: Implement genesis here

  Genesis in paper:

    When we compare a candidate to our own chain, and that candidate forks off
    more than k in the past, we compute the intersection point between that
    candidate and our chain, select s slots from both chains, and compare the
    number of blocks within those s slots. If the candidate has more blocks
    in those s slots, we prefer the candidate, otherwise we stick with our own
    chain.

  Genesis as we will implement it:

    * We decide we are in genesis mode if the head of our chain is more than
      @k@ blocks behind the blockchain time. We will have to approximate this
      as @k/f@ /slots/ behind the blockchain time time.
    * In this situation, we must make sure we have a sufficient number of
      upstream nodes "and collect chains from all of them"
    * We still never consider chains that would require /us/ to rollback more
      than k blocks.
    * In order to compare two candidates, we compute the intersection point of
      X of those two candidates and compare the density at point X.




  Scribbled notes during meeting with Duncan:

   geensis mode: compare clock to our chain
   do we have enough peers?
   still only interested in chains that don't fork more than k from our own chain

     downloading headers from a /single/ node, download at least s headers
     inform /other/ peers: "here is a point on our chain"
     if all agree ("intersection imporved") -- all peers agree
     avoid downloading tons of headers
     /if/ there is a difference, get s headers from the peer who disagrees,
       pick the denser one, and ignore the other
       PROBLEM: what if the denser node has invalid block bodies??
-------------------------------------------------------------------------------}

-- | Clock skew: the number of slots the chain of an upstream node may be
-- ahead of the current slot (according to 'BlockchainTime').
--
-- E.g. a 'ClockSkew' value of @1@ means that a block produced by an upstream
-- it may have a slot number that is 1 greater than the current slot.
newtype ClockSkew = ClockSkew { unClockSkew :: Word64 }
  deriving (Eq, Ord, Enum, Bounded, Show, Num)

type Consensus (client :: * -> * -> (* -> *) -> * -> *) hdr m =
   client hdr (Point hdr) m Void

data ConsensusClientException blk hdr =
      -- | The header we received was for a slot too far in the future.
      --
      -- I.e., the slot of the received header was > current slot (according
      -- to the wall time) + the max clock skew.
      --
      -- The first 'SlotNo' argument is the slot of the received header, the
      -- second 'SlotNo' argument is the current slot.
      TooFarInTheFuture SlotNo SlotNo

      -- | The node we're connecting to forked more than @k@ blocks ago.
      --
      -- We record their current head.
    | ForkTooDeep (Point hdr)

      -- | The ledger threw an error.
    | LedgerError (LedgerError blk)

      -- | The upstream node rolled back more than @k@ blocks.
      --
      -- We store the requested intersection point and head point from the
      -- upstream node.
    | InvalidRollBack (Point hdr) (Point hdr)


deriving instance (StandardHash hdr, Show (LedgerError blk))
    => Show (ConsensusClientException blk hdr)

instance (Typeable hdr, Typeable blk, StandardHash hdr, Show (LedgerError blk))
    => Exception (ConsensusClientException blk hdr)

-- | The state of an upstream node.
data UpstreamState blk hdr = UpstreamState
    { upstreamChain       :: !(ChainFragment hdr)
    , upstreamHeaderState :: !(HeaderState blk)
    }

-- TODO the ChainDB exposes a 'STM m (Set (Point block))'
--
-- ChainSync clients monitor this STM and check its chain as
-- soon as a point is added to this -> check if peer
-- advertises an invalid block, if so throw an exception.
--
--
-- TODO rate-limit switching chains, otherwise we can't place
-- blame (we don't know which candidate's chain included the
-- point that was poisoned). E.g. two rollbacks per time slot
-- -> make it configurable -> just a simple argument for now


-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ConsensusClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
consensusSyncClient :: forall m up blk hdr.
                       ( MonadSTM m
                       , MonadCatch (STM m)
                       , ProtocolLedgerView blk
                       , HasHeader hdr
                       , BlockProtocol hdr ~ BlockProtocol blk
                       , Eq hdr
                       , Typeable hdr
                       , Typeable blk
                       , Ord up
                       )
                    => NodeConfig (BlockProtocol hdr)
                    -> BlockchainTime m
                    -> ClockSkew
                    -> ChainDB m blk hdr (ExtLedgerState blk)
                    -> TVar m (Candidates up hdr)
                    -> up -> Consensus ChainSyncClient hdr m
consensusSyncClient cfg btime (ClockSkew maxSkew) chainDB candidatesVar up =
    ChainSyncClient initialise
  where
    toMaybe :: Either () a -> Maybe a
    toMaybe = either (const Nothing) Just

    initialise :: m (Consensus ClientStIdle hdr m)
    initialise = do
      points <- CF.selectPoints (map fromIntegral offsets) <$>
          atomically (ChainDB.getCurrentChain chainDB)

      return $ SendMsgFindIntersect points $ ClientStIntersect {
          recvMsgIntersectImproved = \intersection _theirHead -> ChainSyncClient $ do
            -- We found an intersection within the last k blocks.
            mbUpstreamStateVar <- fmap toMaybe $ atomically $ runExceptT $ do
              ourChain <- lift $ ChainDB.getCurrentChain chainDB
              -- Roll the chain back to the intersection point. Note that the
              -- max length of the current chain fragment is k, so if the
              -- intersection point is on it, then it is less than k blocks
              -- old.
              upstreamChain <- case CF.rollback intersection ourChain of
                -- If the intersection is not on our chain, it means our
                -- chain has changed since we sent the points, or the peer
                -- sent an invalid response. Either way, try again, by
                -- short-cutting the evaluation. TODO rate limit this?
                Nothing -> throwError ()
                Just c  -> return c
              ledgerState <- lift $ ledgerState <$>
                ChainDB.getCurrentLedger chainDB
              let upstreamHeaderState =
                    getHeaderStateFor ledgerState ourChain upstreamChain
              modifyTVar candidatesVar $
                updateCandidates cfg ourChain (up, upstreamChain)
              newTVar UpstreamState
                { upstreamChain       = upstreamChain
                , upstreamHeaderState = upstreamHeaderState }

            -- Initialise again if the transaction didn't return an
            -- UpstreamState TVar.
            maybe initialise (return . requestNext) mbUpstreamStateVar

        , recvMsgIntersectUnchanged = \theirHead -> ChainSyncClient $ do
            -- If the intersection point is unchanged, this means that the
            -- best intersection point was the initial assumption: genesis.
            upstreamStateVar <- atomically $ do
              ourChain <- ChainDB.getCurrentChain chainDB
              let ourHeadBlockNo = CF.headOrGenBlockNo ourChain
              -- If the genesis point is within k of our own head, this is
              -- fine, but if it is not, we cannot sync with this client.
              when (unBlockNo ourHeadBlockNo > fromIntegral k) $
                throwM $ ForkTooDeep @blk @hdr theirHead

              ledgerState <- ledgerState <$> ChainDB.getCurrentLedger chainDB
              -- The 'HeaderState' at genesis (= ourHeadBlockNo -
              -- ourHeadBlockNo = 0 )
              let rollBack = unBlockNo ourHeadBlockNo
                  upstreamHeaderState = getHeaderState ledgerState rollBack
              modifyTVar candidatesVar $
                updateCandidates cfg ourChain (up, Empty)
              newTVar UpstreamState
                { upstreamChain       = Empty
                , upstreamHeaderState = upstreamHeaderState }
            return $ requestNext upstreamStateVar
        }

    requestNext :: TVar m (UpstreamState blk hdr) -> Consensus ClientStIdle hdr m
    requestNext upstreamStateVar =
        SendMsgRequestNext
          (handleNext upstreamStateVar)
          (return (handleNext upstreamStateVar)) -- case for when we have to wait

    handleNext :: TVar m (UpstreamState blk hdr) -> Consensus ClientStNext hdr m
    handleNext upstreamStateVar = ClientStNext {
        recvMsgRollForward = \hdr theirHead -> ChainSyncClient $ atomically $ do
          currentSlot <- getCurrentSlot btime
          let theirSlot = CF.pointSlot theirHead

          when (unSlotNo theirSlot > unSlotNo currentSlot + maxSkew) $
            throwM $ TooFarInTheFuture @blk @hdr theirSlot currentSlot

          (ourChain, ledgerState, UpstreamState {..}) <- readCurrent upstreamStateVar
          upstreamHeaderState' <-
            case runExcept $ advanceHeader ledgerState hdr upstreamHeaderState of
              Left ledgerError           -> throwM $ LedgerError @blk @hdr ledgerError
              Right upstreamHeaderState' -> return upstreamHeaderState'
          let upstreamChain' = upstreamChain :> hdr
          update upstreamStateVar ourChain upstreamChain' upstreamHeaderState'
          return $ requestNext upstreamStateVar

      , recvMsgRollBackward = \intersection theirHead -> ChainSyncClient $ atomically $ do
          (ourChain, ledgerState, UpstreamState {..}) <- readCurrent upstreamStateVar
          upstreamChain' <- case CF.rollback intersection upstreamChain of
            Just upstreamChain' -> return upstreamChain'
            Nothing             -> throwM $
              InvalidRollBack @blk @hdr intersection theirHead
          let upstreamHeaderState' =
                getHeaderStateFor ledgerState ourChain upstreamChain'
          update upstreamStateVar ourChain upstreamChain' upstreamHeaderState'
          return $ requestNext upstreamStateVar
      }

    -- | Save the updated upstream state and update the candidates accordingly.
    update :: TVar m (UpstreamState blk hdr)
           -> ChainFragment hdr  -- ^ Our chain
           -> ChainFragment hdr  -- ^ Upstream chain
           -> HeaderState blk    -- ^ Upstream 'HeaderState'
           -> STM m ()
    update upstreamStateVar ourChain upstreamChain upstreamHeaderState = do
      writeTVar upstreamStateVar $ UpstreamState
        { upstreamChain       = upstreamChain
        , upstreamHeaderState = upstreamHeaderState }
      modifyTVar candidatesVar $
        updateCandidates cfg ourChain (up, upstreamChain)

    -- | Get the 'HeaderState' for the head of the upstream chain.
    getHeaderStateFor :: LedgerState blk
                      -> ChainFragment hdr  -- ^ Our chain
                      -> ChainFragment hdr  -- ^ Upstream chain
                      -> HeaderState blk
    getHeaderStateFor ledgerState ourChain upstreamChain =
        getHeaderState ledgerState rollBack
      where
        ourHeadBlockNo      = CF.headOrGenBlockNo ourChain
        upstreamHeadBlockNo = CF.headOrGenBlockNo upstreamChain
        rollBack = unBlockNo ourHeadBlockNo - unBlockNo upstreamHeadBlockNo

    -- | Read the current chain, ledger state, and upstream state.
    readCurrent :: TVar m (UpstreamState blk hdr)
                -> STM m ( ChainFragment hdr
                         , LedgerState blk
                         , UpstreamState blk hdr)
    readCurrent upstreamStateVar = (,,)
              <$> ChainDB.getCurrentChain chainDB
              <*> (ledgerState <$> ChainDB.getCurrentLedger chainDB)
              <*> readTVar upstreamStateVar

    -- Recent offsets
    --
    -- These offsets are used to find an intersection point between our chain
    -- and the upstream node's. We use the fibonacci sequence to try blocks
    -- closer to our tip, and fewer blocks further down the chain. It is
    -- important that this sequence constains at least a point @k@ back: if
    -- no intersection can be found at most @k@ back, then this is not a peer
    -- that we can sync with (since we will never roll back more than @k).
    --
    -- For @k = 2160@, this evaluates to
    --
    -- > [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2160]
    --
    -- For @k = 5@ (during testing), this evaluates to
    --
    -- > [0,1,2,3,5]
    offsets :: [Word64]
    offsets = [0] ++ takeWhile (< k) [fib n | n <- [2..]] ++ [k]

    k :: Word64
    k = maxRollbacks $ protocolSecurityParam cfg

{-------------------------------------------------------------------------------
  Chain candidates
-------------------------------------------------------------------------------}

newtype Candidates up hdr = Candidates {
      -- | Candidate chains
      --
      -- We track for each upstream node the /candidate/ chain that the chain
      -- selection rule tells us is preferred over our current chain.
      candidates :: Map up (ChainFragment hdr)
    }

instance (Condense up, Condense hdr, HasHeader hdr) => Condense (Candidates up hdr) where
  condense (Candidates cs) = condense cs


noCandidates :: Ord up => Candidates up hdr
noCandidates = Candidates mempty

-- | Update candidates
updateCandidates :: ( OuroborosTag (BlockProtocol hdr)
                    , HasHeader hdr
                    , Eq        hdr
                    , Ord up
                    )
                 => NodeConfig (BlockProtocol hdr)
                 -> ChainFragment hdr       -- ^ Our chain
                 -> (up, ChainFragment hdr) -- ^ New potential candidate
                 -> Candidates up hdr -> Candidates up hdr
updateCandidates cfg ourChain (up, theirChain) (Candidates cands)
    | preferCandidate cfg ourChain theirChain
    = Candidates $ Map.insert up theirChain cands
    | otherwise
    = Candidates $ Map.delete up            cands
    -- We unconditionally remove the old chain
    --
    -- This means that /if/ we for some reason prefer the old candidate from
    -- this node but not the new, we nonetheless forget this old candidate.
    -- This is reasonable: the node might not even be able to serve this
    -- old candidate anymore.
    --
    -- TODO: Discuss this with Duncan.
