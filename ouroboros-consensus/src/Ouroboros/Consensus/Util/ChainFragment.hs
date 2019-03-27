-- | Utility functions on chain fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.ChainFragment as CF
module Ouroboros.Consensus.Util.ChainFragment (
    forksAtMostKBlocks
  ) where

import           Prelude hiding (length)

import           Data.Word (Word64)

import           Ouroboros.Network.ChainFragment

{-------------------------------------------------------------------------------
  Utility functions on chain fragments
-------------------------------------------------------------------------------}

forksAtMostKBlocks :: HasHeader b
                   => Word64           -- ^ How many blocks can it fork?
                   -> ChainFragment b  -- ^ Our chain.
                   -> ChainFragment b  -- ^ Their chain.
                   -> Bool             -- ^ Indicates whether their chain
                                       -- forks at most the specified number
                                       -- of blocks.
forksAtMostKBlocks k ours theirs
    | Just (_, _, ourSuffix, _) <- intersectChainFragments ours theirs
    = fromIntegral (length ourSuffix) <= k
    | otherwise
    = False
