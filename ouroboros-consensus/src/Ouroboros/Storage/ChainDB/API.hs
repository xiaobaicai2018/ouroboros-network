{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Storage.ChainDB.API
  ( ChainDB(..)
  ) where

import           Control.Monad.Class.MonadSTM

import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (HasHeader, Slot)
import           Ouroboros.Network.Chain (Point)

-- TODO This is all obsolete since the new requirements!
--
-- Blocks might arrive out of order -> they might not be part of any chain
-- (yet) when downloading from multiple peers. They are also not validated.
--
-- Adding a block will write it directly into the VolatileDB instead of
-- pushing it onto the (in-memory) chain. So it can no longer be a
-- transaction. The MemoryDB will have to get rid of the map.
--
-- There needs to be a new function (STM) that pushes a block by reference to
-- the current chain (ChainFragment).
--
-- TODO Provide a function: getBlock that throws an error when the block
-- doesn't exist. This will be used by the LedgerDB that knows for sure a
-- block must exist. The ChainStateDB will then do validation/truncation.



-- | TODO
--
-- In addition to storing the contents of the 'ImmutableDB' and 'VolatileDB',
-- the 'ChainDB' also stores the __current__ chain.
data ChainDB block m = ChainDB
  { -- | Resolve a @'Point' block@ (reference to a block) to a block.
    --
    -- Throws a 'MissingBlockError' if there is no @block@ with the given
    -- @'Point' block@.
    getBlock       :: (HasCallStack, HasHeader block) => Point block -> m block

    -- | Add a block to the end of the current chain.
  , addBlock       :: HasHeader block => block -> Tr m ()

    -- | Switch to a fork by rolling back a number (@rollBack@) of blocks and
    -- applying new blocks.
    --
    -- Precondition: @rollBack <= k && length newBlocks >= rollBack@. We never
    -- roll back more than @k@ blocks. We also never switch to a shorter fork.
    --
    -- TODO check the precondition for off-by-one errors.
  , switchFork     :: HasHeader block
                   => Int      -- ^ @rollBack@
                   -> [block]  -- ^ @newBlocks@ TODO no longer by value, but
                               -- by ref, so either @header@ or @Point block@.
                   -> Tr m ()

    -- | Get the tip of the current chain.
  , getTip         :: HasHeader block => Tr m (Point block)
    -- TODO Alternatively return a list of 'Point's or a 'ChainFragment' of
    -- the block headers.

    -- | Get the block stored at the given 'Slot' in the current chain.
    --
    -- Returns 'Nothing' when the slot is unfilled.
    --
    -- Throws a 'ReadFutureSlotError' if the slot is greater than the slot of
    -- the tip.
  , getBlockBySlot :: (HasCallStack, HasHeader block)
                   => Slot -> m (Maybe block)

    -- TODO add the ability to efficiently stream the contents of the current
    -- chain (or any chain identified by a 'Point'?) of the 'ChainDB'. This
    -- can be used to write a function that converts a 'ChainDB' to a 'Chain',
    -- useful for testing purposes.
  }

