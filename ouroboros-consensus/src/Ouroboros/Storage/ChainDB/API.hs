{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

{-
To support the old Byron logic layer we need:
- `getBinaryBlob`, because Byron needs a `ConduitT () ByteString IO ()` and so can't do safe iterator bracketing.
- `getTip`, because Byron needs `m Block`
To support a new chain sync server we need:
- The current iterator style will work, or something like it: read blocks from a given point.
- A blocking read, for the case in which the reader is at the tip of chain and we need to send the new tip block once it comes in.
- The above 2 items need to also show when there's a fork
-}

module Ouroboros.Storage.ChainDB.API (
    -- * Main ChainDB API
    ChainDB(..)
  , getTipHeader
  , getTipPoint
    -- * Iterator API
  , StreamFrom(..)
  , StreamTo(..)
  , Iterator(..)
  , IteratorID(..)
  , IteratorResult(..)
  ) where

import           Control.Monad.Class.MonadSTM

import           Data.Set (Set)

import           Ouroboros.Network.Block (HasHeader (..))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Point (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as Fragment

data ChainDB m block hdr l =
    ( HasHeader block
    , HasHeader hdr
    , HeaderHash block ~ HeaderHash hdr
    ) => ChainDB {
      -- | Add a block to the heap of blocks
      --
      -- The block /must/ correspond to the specified point; we cannot verify
      -- this in the chain DB itself, so this must be guaranteed externally.
      --
      -- We do /not/ assume that the block is valid (under the legder rules);
      -- it is the responsibility of the Chain DB itself to only select chains
      -- that are valid.
      --
      -- Conversely, the caller cannot assume that the new block will be added
      -- to the current chain; even if the block is valid, it will not become
      -- part of the chain if there are other chains available that are
      -- preferred by the consensus algorithm (typically, longer chains).
      addBlock        :: Point block -> block -> m ()

      -- | Get the current chain
      --
      -- Returns a chain fragment at least @k@ long (unless chain near genesis).
      -- The chain is guaranteed to be non-empty (as it will at the very minimum
      -- contain the EBB for epoch 0).
    , getCurrentChain :: STM m (ChainFragment hdr)

      -- | Get block by point
      --
      -- This reads from disk and therefore cannot be transactional.
    , getBlock        :: Point block -> m (Maybe block)

      -- | Get block at the tip of the chain
      --
      -- The Chain DB ensures that there is always at least one block on the
      -- chain (the EBB for epoch 0). This is defined as part of the API,
      -- rather than derived, to avoid client code having to deal with the
      -- (non-) possibility of an error when first doing `getTipPoint` and
      -- then a separate `getBlock`.
    , getTipBlock     :: m block

      -- | TODO
    , getIsFetched    :: STM m (Point block -> Bool)

      -- | TODO
    , getCurrentLedger :: STM m l

      -- | Stream blocks
    , streamBlocks    :: StreamFrom block -> StreamTo block -> m (Iterator m block)

      -- | TODO
    , getPoisonedPoints :: STM m (Set (Point block))
    }

getTipHeader :: MonadSTM m => ChainDB m block hdr l -> STM m hdr
getTipHeader ChainDB{..} =
    mustExist . Fragment.head <$> getCurrentChain
  where
    mustExist :: Maybe a -> a
    mustExist (Just a) = a
    mustExist Nothing  = error "invariant violation: empty current chain"

getTipPoint :: MonadSTM m => ChainDB m block hdr l -> STM m (Point block)
getTipPoint chainDB@ChainDB{..} =
    castPoint . Chain.blockPoint <$> getTipHeader chainDB

{-------------------------------------------------------------------------------
  Iterator API
-------------------------------------------------------------------------------}

data StreamFrom block =
    StreamFromInclusive (Point block)
  | StreamFromExclusive (Point block)
  | StreamFromGenesis

data StreamTo block =
    StreamToInclusive (Point block)
  | StreamToExclusive (Point block)
  | StreamToEnd

data Iterator m block = Iterator {
      iteratorNext  :: m (IteratorResult block)
    , iteratorClose :: m ()
    , iteratorID    :: IteratorID
    }

newtype IteratorID = IteratorID Int

data IteratorResult block =
    IteratorExhausted
  | IteratorResult block

{-------------------------------------------------------------------------------
  Network auxiliary
-------------------------------------------------------------------------------}

castPoint :: HeaderHash x ~ HeaderHash y => Point x -> Point y
castPoint Point{..} = Point pointSlot (Block.castHash pointHash)

