{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Storage.ChainDB.MemoryDB
    ( MemoryDB
    , empty
    , addBlock
    , rollBack
    , switchFork
    , getTip
    , getTip'
    , getOldest
    , getBlock
    , getSlot
    , chainLength
    , blocksStored
    , extractOldestBlock
    ) where

import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybe)

import           Ouroboros.Network.Block (HasHeader (..), Hash (..), Slot)
import           Ouroboros.Network.Chain (genesisPoint)
import           Ouroboros.Network.ChainFragment (ChainFragment, Point (..))
import qualified Ouroboros.Network.ChainFragment as CF

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))


{------------------------------------------------------------------------------
  MemoryDB
------------------------------------------------------------------------------}

data MemoryDB header block = MemoryDB
  { _memChain         :: !(ChainFragment header)
  , _memBlocks        :: !(Map (Point block) block)
  , _memGetHeader     :: !(block -> header)
  , _memSecurityParam :: !SecurityParam
  }

empty :: HasHeader header
      => (block -> header)
      -> SecurityParam
      -> MemoryDB header block
empty _memGetHeader _memSecurityParam  = MemoryDB
    { _memChain     = CF.Empty
    , _memBlocks    = Map.empty
    , ..
    }

-- Only the last @k@ headers are stored in the 'ChainFragment'
addBlock :: (HasHeader header, HasHeader block)
         => block
         -> MemoryDB header block
         -> MemoryDB header block
addBlock block MemoryDB{..} = MemoryDB
    -- TODO can we trim here (takeNewest), or should we wait for the workers for some reason?
    -- -> the trimmed blocks should end up in the ImmutableDB, via the VolatileDB
    { _memChain  = CF.takeNewest k $ CF.addBlock header _memChain
    , _memBlocks = Map.insert (CF.blockPoint block) block _memBlocks
    , ..
    }
  where
    header = _memGetHeader block
    k = fromIntegral $ maxRollbacks _memSecurityParam

rollBack :: HasHeader header
         => Int
         -> MemoryDB header block
         -> MemoryDB header block
rollBack n MemoryDB{..} = MemoryDB
    { _memChain = CF.dropNewest n _memChain
    , ..
    }

switchFork :: (HasHeader header, HasHeader block)
           => Int
           -> [block]
           -> MemoryDB header block
           -> MemoryDB header block
switchFork n newBlocks = addBlocks . rollBack n
  where
    addBlocks memDB = foldl' (flip addBlock) memDB newBlocks

getTip :: (HasHeader header, HeaderHash header ~ HeaderHash block)
       => MemoryDB header block
       -> Maybe (Point block)
getTip = fmap pointFromHeader . CF.head . _memChain

getTip' :: (HasHeader header, HeaderHash header ~ HeaderHash block)
        => MemoryDB header block
        -> Point block
getTip' = maybe genesisPoint pointFromHeader . CF.head . _memChain

getOldest :: (HasHeader header, HeaderHash header ~ HeaderHash block)
          => MemoryDB header block
          -> Point block
getOldest = maybe genesisPoint pointFromHeader . CF.last . _memChain

getBlock :: HasHeader block
         => Point block
         -> MemoryDB header block
         -> Maybe block
getBlock point MemoryDB{..} = Map.lookup point _memBlocks

pointFromHeader :: (HasHeader header, HeaderHash header ~ HeaderHash block)
                => header
                -> Point block
pointFromHeader header = Point
    { pointSlot = blockSlot header
    , pointHash = BlockHash (blockHash header)
    }

getSlot :: (HasHeader header, HeaderHash header ~ HeaderHash block)
        => Slot
        -> MemoryDB header block
        -> Maybe (Point block)
getSlot slot MemoryDB{..} = pointFromHeader <$> CF.lookupBySlot _memChain slot

chainLength :: HasHeader header => MemoryDB header block -> Int
chainLength = CF.length . _memChain

-- | Return the number of blocks stored in the blocks cache.
blocksStored :: MemoryDB header block -> Int
blocksStored = Map.size . _memBlocks

-- | Return the block with the smallest 'Slot' (= the oldest) from the
-- 'MemoryDB' together with the 'MemoryDB' without the block in question.
--
-- Return 'Nothing' when no blocks are stored in the 'MemoryDB'.
--
-- Note: this operation completely ignores the 'ChainFragment', which only
-- stores block headers.
extractOldestBlock :: MemoryDB header block
                   -> Maybe (block, MemoryDB header block)
extractOldestBlock memDB@MemoryDB{..}
    | Just (block, blocks') <- Map.minView _memBlocks
      -- Note: we rely on the fact that 'pointSlot' is the first field of
      -- 'Point' for the derived 'Ord'-instance.
    = Just (block, memDB { _memBlocks = blocks' })
    | otherwise
    = Nothing
