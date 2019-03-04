{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | 'ChainDB' implementation that stores the current chain, forks, and
-- blocks.
--
-- Immutable blocks, i.e. blocks older than @k@ (blocks old), are stored in
-- the 'ImmutableDB', newer blocks that are not yet part of /the/ chain,
-- including blocks part of different forks, are stored in the 'VolatileDB'.
-- Most operations, e.g., adding a block, switching fork, etc., don't write
-- directly to disk, but are first applied to an in-memory database
-- ('MemoryDB').This means that most operations will be fast, as they won't
-- require blocking disk access.
--
-- The in-memory database 'MemoryDB' consists of a 'ChainFragment' that acts
-- as the \"tail\" of the chain. Adding blocks and switching forks are
-- performed on this 'ChainFragment' directly. This 'ChainFragment' does not
-- store full blocks but only the headers of these blocks, because it will
-- contain at least the last @k@ (or the total chain, if it is shorter than
-- @k@) headers, so we can efficiently switch forks and get the tip of the
-- chain. If we were to store the last @k@ /blocks/ (instead of headers), we
-- would use way too much memory (@k * ~2 MiB@).
--
-- In addition to this 'ChainFragment', the 'MemoryDB' also stores a map of
-- blocks (from @'Point' block@ to @block@) to which blocks are also added
-- when they are added to the chain. Note that this map will also store blocks
-- that /were/ part of the 'ChainFragment', but are no longer as they were
-- part of a fork that has been switched away from since. It is important to
-- keep track of these blocks, so that the background worker has the chance to
-- store them in the 'VolatileDB'.
--
-- There is a background worker that continuously takes a block out of the
-- 'MemoryDB' and writes it to the 'VolatileDB'. When too many blocks are
-- being added to the 'MemoryDB', e.g., when we're switching between long
-- forks, adding blocks will block until the background worker caught up. The
-- maximum number of blocks in the 'MemoryDB' can be configured with TODO.
--
-- >      ┏━━━━━━━━━━┓      ┏━━━━━━━━━━━━┓      ┏━━━━━━━━━━━━━┓
-- > ───▶ ┃ MemoryDB ┃ ───▶ ┃ VolatileDB ┃ ───▶ ┃ ImmutableDB ┃
-- >  1.  ┗━━━━━━━━━━┛  2.  ┗━━━━━━━━━━━━┛  3.  ┗━━━━━━━━━━━━━┛
--
-- 1. Most functions operate directly on the 'MemoryDB'.
--
-- 2. A first background worker continuously writes blocks from the 'MemoryDB'
--    to the 'VolatileDB'.
--
-- 3. A second background worker periodically writes blocks from the
--    'VolatileDB' to the 'ImmutableDB'. If all blocks from the 'VolatileDB'
--    older than some @slot@ are written to the 'ImmutableDB' at time @t@,
--    then a garbage collection for this @slot@ will be performed on the
--    'VolatileDB' at time @t + interval@, giving the OS some time to flush
--    the writes to 'ImmutableDB' to disk before we remove them from the
--    'VolatileDB', so that we don't lose blocks in case of crash.
--
--
-- = Invariants
--
-- The following invariants that will always hold, whether the background
-- workers have run or not:
--
--   [Invariant /immAtLeastK/] all the blocks in the 'ImmutableDB' are at
--   least @k@ (blocks) older than the newest block stored in the entire
--   'ChainDB'.
--
--   What might happen is that some blocks that are @k@ blocks old (or older)
--   are not yet stored in the 'ImmutableDB'. This will be rectified as soon
--   as the right background worker runs, see /OlderThanKImm/.
--
--   While opening the 'ChainDB', an inspection of the on-disk files might
--   reveal that this invariant does not hold for the current on-disk files,
--   e.g., when the 'VolatileDB' is corrupted or erased. In that case, before
--   the 'ChainDB' is actually opened, blocks are moved from the 'ImmutableDB'
--   to the 'VolatileDB' so that the invariant will hold after all.
--
--
--   [Invariant /VolAtLeastK/] the 'VolatileDB' must store at least @k@
--   blocks, unless the total number of blocks in the 'ChainDB' is less than
--   @k@.
--
--   In most cases, the 'VolatileDB' will store more than @k@ blocks for
--   multiple reasons:
--
--   * It will store multiple forks, so that there can more than @k@ blocks,
--     which does not necessarily mean some of these are @k@ old.
--
--   * The (second) background worker, which will garbage collect blocks older
--     than @k@, has not yet run. Garbage collection is done for the
--     'VolatileDB' after blocks older than @k@ have been copied to the
--     'ImmutableDB'. To give the OS some time to flush the blocks appended to
--     the 'ImmutableDB' to disk, we don't garbage collect directly after
--     appending, but wait a bit. Additionally, garbage collection might not
--     immediately delete all blocks older than @k@ from the 'VolatileDB', as
--     they might be stored in files that contain newer blocks, and only whole
--     files are deleted.
--
--   [Invariant /memAtLeastK/] the 'MemoryDB' must store at least @k@ headers
--   in its 'ChainFragment', unless the total number of blocks in the
--   'ChainDB' is less than @k@.
--
-- Each time the background workers have run, the following invariants should
-- also hold:
--
--   [Invariant /OlderThanKImm/] every block at least @k@ (blocks) older than
--   the newest block stored in the entire 'ChainDB' must be stored in the
--   'ImmutableDB' (but can also be stored in the other data structures).
--
-- TODO add an invariant for or derive from others: the 'ChainFragment' in the
-- 'MemoryDB' will never store a header of a block that is in the
-- 'ImmutableDB'
--
-- TODO for all blocks: either its header is in the 'MemoryDB' or it is in the
-- 'ImmutableDB'.
module Ouroboros.Storage.ChainDB.Impl
  ( mkChainDB

    -- * Invariants for testing purposes
  , immAtLeastK
  , volAtLeastK
  , memAtLeastK
  , olderThanKImm
  ) where

import           Codec.Serialise (Serialise, deserialiseOrFail)
import           Control.Monad.Class.MonadSTM

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Function ((&))

import           GHC.Stack (HasCallStack, callStack)

import           Ouroboros.Network.Block (HasHeader (..), Hash (..), Slot)
import           Ouroboros.Network.ChainFragment (Point (..))

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.ChainDB.MemoryDB (MemoryDB)
import qualified Ouroboros.Storage.ChainDB.MemoryDB as MemDB
import           Ouroboros.Storage.ChainDB.Types
import           Ouroboros.Storage.ImmutableDB.API (ImmutableDB)
import qualified Ouroboros.Storage.ImmutableDB.API as ImmDB
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import           Ouroboros.Storage.VolatileDB.API (VolatileDB)
import qualified Ouroboros.Storage.VolatileDB.API as VolDB




{------------------------------------------------------------------------------
  ChainDB
------------------------------------------------------------------------------}


data ChainDBEnv header block m = ChainDBEnv
    { _dbErr           :: !(ErrorHandling (ChainDBError block) m)
    , _dbImmDB         :: !(ImmutableDB              m)
    , _dbVolDB         :: !(VolatileDB (Point block) m)
    , _dbMemDB         :: !(TVar m (MemoryDB header block))
    , _dbMemMaxBlocks  :: !Int
      -- ^ The maximum number of blocks the 'MemoryDB' may store in memory.
      -- Important: this must be >= '_dbSecurityParam', otherwise we cannot
      -- switch forks of length '_dbSecurityParam' (the transaction will keep
      -- on retrying).
    , _dbSecurityParam :: !SecurityParam
    }

mkChainDB :: CanChainDB header block m
          => ChainDBEnv header block m
          -> ChainDB           block m
mkChainDB dbEnv = ChainDB
    { getBlock       = getBlockImpl       dbEnv
    , addBlock       = addBlockImpl       dbEnv
    , switchFork     = switchForkImpl     dbEnv
    , getTip         = getTipImpl         dbEnv
    , getBlockBySlot = getBlockBySlotImpl dbEnv
    }

-- SecurityParam k: the number of /blocks/, no /slots/ we can roll back. We
-- use this to determine which block should be written to the immutableDB (and
-- then GC'ed from the volatile).
--
-- Can we use the following for this:
--
-- blockNo        :: b -> BlockNo
--
-- TODO confirm with Duncan: is this correct (and monotonically increasing)?


{------------------------------------------------------------------------------
  Implementation
------------------------------------------------------------------------------}

type CanChainDB header block m =
  (HasCallStack, MonadSTM m, HasHeader block, HasHeader header,
   HeaderHash header ~ HeaderHash block, Serialise block)

-- Based on the 'Point', we can't tell whether the corresponding block will be
-- stored in 'MemoryDB', the 'VolatileDB', or the 'ImmutableDB', as the
-- 'Point' does not give us the block number. So try them each in turn.
getBlockImpl :: forall header block m. (CanChainDB header block m)
             => ChainDBEnv header block m
             -> Point block
             -> m block
getBlockImpl ChainDBEnv{..} point@Point{..} = firstMatch
    [ MemDB.getBlock               point     <$> atomically (readTVar _dbMemDB)
    , VolDB.getBlock      _dbVolDB point     >>= traverse (deserialise _dbErr)
    , ImmDB.getBinaryBlob _dbImmDB pointSlot >>= traverse (deserialise _dbErr) &
      fmap (>>= checkHash) -- The lookup in the 'ImmutableDB' is only by
                           -- 'Slot', so we need to check the hash of the
                           -- 'Point' as well.
    ]
  where
    ErrorHandling{..} = _dbErr

    -- | Return the first @Just@ in the list. If they all are @Nothing@ or if
    -- the list is empty, throw a 'MissingBlockError'.
    firstMatch :: HasCallStack => [m (Maybe block)] -> m block
    firstMatch []      = throwError $ MissingBlockError (Left point) callStack
    firstMatch (m:ms') = do
      mbBlock <- m
      case mbBlock of
        Just block -> return block
        Nothing    -> firstMatch ms'

    -- | If the block's hash (found by its 'Slot') doesn't match that of the
    -- @point@, return @Nothing@, otherwise @Just@.
    checkHash :: block -> Maybe block
    checkHash block
      | BlockHash (blockHash block) == pointHash
      = Just block
      | otherwise
      = Nothing

addBlockImpl :: CanChainDB header block m
             => ChainDBEnv header block m
             -> block
             -> Tr m ()
addBlockImpl ChainDBEnv{..} block = do
    memDB <- readTVar _dbMemDB
    check (MemDB.blocksStored memDB + 1 <= _dbMemMaxBlocks)
    let !memDB' = MemDB.addBlock block memDB
    writeTVar _dbMemDB memDB'

switchForkImpl :: CanChainDB header block m
               => ChainDBEnv header block m
               -> Int
               -> [block]
               -> Tr m ()
switchForkImpl ChainDBEnv{..} rollBack newBlocks = do
    -- TODO rollBack <= k
    -- TODO length newBlocks >= rollBack
    -- throw an -Error, an 'error', or 'assert'?
    let n = length newBlocks
    memDB <- readTVar _dbMemDB
    -- Check if we may store that many extra blocks in memory
    check (MemDB.blocksStored memDB + n <= _dbMemMaxBlocks)
    let !memDB' = MemDB.switchFork rollBack newBlocks memDB
    writeTVar _dbMemDB memDB'

getTipImpl :: CanChainDB header block m
           => ChainDBEnv header block m
           -> Tr m (Point block)
getTipImpl ChainDBEnv{..} =
    MemDB.getTip' <$> readTVar _dbMemDB

getBlockBySlotImpl :: forall m block header. CanChainDB header block m
                   => ChainDBEnv header block m
                   -> Slot
                   -> m (Maybe block)
getBlockBySlotImpl ChainDBEnv{..} slot = do
    memDB <- atomically $ readTVar _dbMemDB

    let tipSlot = pointSlot $ MemDB.getTip'   memDB
        minSlot = pointSlot $ MemDB.getOldest memDB

    if
      | slot > tipSlot  ->
        throwError $ ReadFutureSlotError slot tipSlot callStack

      | slot >= minSlot ->
        -- If the slot is filled, it will be in the in-memory 'ChainFragment'.
        case MemDB.getSlot slot memDB of
          -- Unfilled slot
          Nothing -> return Nothing
          Just pt -> case MemDB.getBlock pt memDB of
            -- The block was still stored in the 'MemoryDB'
            Just block -> return (Just block)

            -- The block has already been moved to the 'VolatileDB'.
            Nothing    -> VolDB.getBlock _dbVolDB pt >>= \case
              Just block -> Just <$> deserialise _dbErr block
              -- This cannot happen
              Nothing    -> error $
                "Block missing from the VolatileDB: " <> show pt

-- TODO problem: if we trim the ChainFragment, we don't have the point of the
-- block, which we need to look it up in the VolatileDB

        -- It must already be stored in 'ImmutableDB'.
      | otherwise -> ImmDB.getBinaryBlob _dbImmDB slot >>=
        traverse (deserialise _dbErr)
  where
    ErrorHandling{..} = _dbErr


{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

-- | Deserialise the given 'ByteString' to a @block@.
--
-- Throw a 'DeserialiseError' in case deserialisation fails.
deserialise :: (Serialise block, Monad m)
            => ErrorHandling (ChainDBError block) m
            -> ByteString
            -> m block
deserialise ErrorHandling{..} bs = case deserialiseOrFail (BL.fromStrict bs) of
    Left  df    -> throwError $ DeserialiseError df callStack
    Right block -> return block


{------------------------------------------------------------------------------
  Invariants
------------------------------------------------------------------------------}

immAtLeastK :: CanChainDB header block m
            => ChainDBEnv header block m
            -> m Bool
immAtLeastK dbEnv@ChainDBEnv{..} = do

    -- TODO more precise error reporting, use a separate error for this?

    -- TODO check all blocks instead of assuming that 'blockNo's are
    -- monotonically increasing?

    tipPoint <- atomically $ getTipImpl dbEnv
    tipBlock <- getBlockImpl dbEnv tipPoint

    let newestBlockNo = blockNo tipBlock
        k = maxRollbacks _dbSecurityParam
        maxBlockNo = toEnum $ fromEnum newestBlockNo - fromIntegral k

    immNextSlot <- ImmDB.getNextSlot _dbImmDB

    case immNextSlot of
      -- If the ImmutableDB is empty, the invariant is trivially true
      0    -> return True
      next -> do
        lastBlob <- ImmDB.getBinaryBlob _dbImmDB (next - 1)
        case lastBlob of
          -- The tip of the ImmutableDB is never an unfilled slot
          Nothing   -> return False
          Just blob -> do
            lastBlockNo <- blockNo <$> deserialise _dbErr blob
            return $ lastBlockNo <= maxBlockNo


volAtLeastK :: CanChainDB header block m
            => ChainDBEnv header block m
            -> m Bool
volAtLeastK = undefined
   -- TODO how can we know the total number of blocks in the 'VolatileDB'?


memAtLeastK :: CanChainDB header block m
            => ChainDBEnv header block m
            -> m Bool
memAtLeastK dbEnv@ChainDBEnv{..} = do
    -- TODO isn't chainLength always <= k?
    let k = fromIntegral $ maxRollbacks _dbSecurityParam

    chainLength <- MemDB.chainLength <$> atomically (readTVar _dbMemDB)
    totalBlocks <- totalBlockStored dbEnv

    return $ chainLength >= (totalBlocks `min` k)


olderThanKImm :: CanChainDB header block m
              => ChainDBEnv header block m
              -> m Bool
olderThanKImm = undefined
     -- TODO go over all blocks (or their headers) in the 'MemoryDB' and
     -- 'VolatileDB' and check that if the block is older than k, it is also
     -- present in the 'ImmutableDB'.

totalBlockStored :: CanChainDB header block m
                 => ChainDBEnv header block m
                 -> m Int
totalBlockStored = undefined -- TODO
