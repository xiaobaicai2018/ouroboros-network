{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-- |

A mock FS implementation, suitable for testing.

--}

module Ouroboros.Storage.FS.Sim (
    -- * Mock FS implementation & monad
      SimFS
    , SimFSE
    , MockHandle(..)
    , MockFS(..)
    , newEmptyMockFS
    , runSimFS
    , prettyShowFS
    -- * Testing examples
    , mockDemo
    , mockDemoScript
    , demoMockFS
    ) where

import           Control.Monad.Catch (MonadCatch (..), MonadMask (..),
                     MonadThrow (..), bracket)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           GHC.Generics (Generic)
import           GHC.Stack
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Bifunctor (second)
import           Data.Functor.Identity
import           Data.List (foldl')
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Data.Word (Word64)
import           System.IO (IOMode, SeekMode)
import qualified System.IO as IO

import           Ouroboros.Network.MonadClass

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree)
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

type SimFSE m = ExceptT FsError (SimFS m)

newtype SimFS m a =
    SimFS { unSimFs :: ReaderT (TVar m MockFS) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

instance MonadSTM m => MonadState MockFS (SimFS m) where
  state f = SimFS $ ReaderT $ \stateVar -> atomically $ do
      fs <- readTVar stateVar
      let (a, fs') = f fs
      writeTVar stateVar fs'
      return a

instance MonadTrans SimFS where
    lift = SimFS . lift

newtype TrSimFS m a = TrSimFS (m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans TrSimFS where
    lift = TrSimFS

instance MonadFork m => MonadFork (SimFS m) where
  fork (SimFS f) = SimFS $ ReaderT $ \e -> fork (runReaderT f e)

instance (MonadFork (SimFS m) , MonadSTM m) => MonadSTM (SimFS m) where
  type Tr (SimFS m)      = TrSimFS (Tr m)
  type TVar (SimFS m)    = TVar m
  type TMVar (SimFS m)   = TMVar m
  type TBQueue (SimFS m) = TBQueue m

  atomically (TrSimFS t) = lift $ atomically t
  newTVar                = lift . newTVar
  readTVar               = lift . readTVar
  writeTVar t a          = lift $ writeTVar t a
  retry                  = lift retry

  newTMVar               = lift . newTMVar
  newTMVarIO             = lift . newTMVarIO
  newEmptyTMVar          = lift newEmptyTMVar
  newEmptyTMVarIO        = lift newEmptyTMVarIO
  takeTMVar              = lift . takeTMVar
  tryTakeTMVar           = lift . tryTakeTMVar
  putTMVar    t a        = lift $ putTMVar t a
  tryPutTMVar t a        = lift $ tryPutTMVar t a
  swapTMVar   t a        = lift $ swapTMVar t a
  readTMVar              = lift . readTMVar
  tryReadTMVar           = lift . tryReadTMVar
  isEmptyTMVar           = lift . isEmptyTMVar

  newTBQueue             = lift . newTBQueue
  readTBQueue            = lift . readTBQueue
  writeTBQueue q a       = lift $ writeTBQueue q a
#if MIN_VERSION_stm(2,5,0)
  lengthTBQueue          = lift . lengthTBQueue
#endif


{-------------------------------------------------------------------------------
  Mock FS types
-------------------------------------------------------------------------------}

data MockFS = MockFS {
      mockFiles      :: MockFiles
    , mockHandles    :: Map MockHandle MockHandleState
    , mockNextHandle :: MockHandle
    }
  deriving (Generic, Show)

-- | A mock handle to a file on disk.
--
-- This is only meaningful when interpreted against a 'MockFS'.
newtype MockHandle = MockHandle Int
  deriving (Show, Eq, Ord, Enum)

-- | Mock handle internal state
--
-- TODO: Shouldn't we split this into 'MockHandleOpen' and 'MockHandleClosed'?
-- We currently cannot simulate handle closure.
data MockHandleState = MockHandleState {
      mhFilePath :: FsPath
    , mhIOMode   :: IOMode
    , mhOffset   :: DiskOffset
    }
  deriving (Show)

type DiskOffset = Word64


type MockFiles = FsTree ByteString

{-------------------------------------------------------------------------------
  Internal utilities for implementing the mock FS
-------------------------------------------------------------------------------}

-- | Monads in which we can simulate the file system
type CanSimFS m = ( HasCallStack
                  , MonadState MockFS m
                  , MonadError FsError m
                  )

-- | Convenience combinator for functions that modify the mock file system.
modifyMockFS :: CanSimFS m => (MockFS -> Except FsError (a, MockFS)) -> m a
modifyMockFS f = do
    st <- get
    case runExcept $ f st of
      Left  err      -> throwError err
      Right (a, st') -> put st' >> return a

-- | Convenience combinators for functions that read but not modify the mock FS.
readMockFS :: CanSimFS m => (MockFiles -> Except FsError a) -> m a
readMockFS f = modifyMockFS (\fs -> (, fs) <$> f (mockFiles fs))

-- | Convenience combinator for functions that modify the contents of the mock
-- file system through a file handle.
withHandleModify :: CanSimFS m
                 => MockHandle
                 -> (    MockFiles
                      -> MockHandleState
                      -> Except FsError (a, (MockFiles, MockHandleState))
                    )
                 -> m a
withHandleModify h f = do
    st <- get
    case M.lookup h (mockHandles st) of
      Just hs ->
        case runExcept $ f (mockFiles st) hs of
          Left err ->
            throwError err
          Right (a, (fs', hs')) -> do
            put $ st { mockHandles = M.insert h hs' (mockHandles st)
                     , mockFiles   = fs'
                     }
            return a
      Nothing ->
        error "withHandleModify: handle not found"


-- | Convenience combinator for functions that need a file handle but do not
-- modify the contents of the file system.
withHandleRead :: CanSimFS m
               => MockHandle
               -> (    MockFiles
                    -> MockHandleState
                    -> Except FsError (a, MockHandleState)
                  )
               -> m a
withHandleRead h f = withHandleModify h $ \fs hs -> second (fs, ) <$> f fs hs

throwFsError :: (HasCallStack, MonadError FsError m)
             => FsErrorType -> Maybe FsPath -> m a
throwFsError et fp = throwError (FsError et fp callStack)

{------------------------------------------------------------------------------
 Mock FS implementation
------------------------------------------------------------------------------}

instance (MonadMask m, MonadSTM m) => HasFS (SimFSE m) where
    type FsHandle (SimFSE m) = MockHandle
    data Buffer   (SimFSE m) = MockBufferUnused

--    dumpState  = mockDumpState
--    newBuffer  = mockNewBuffer
    hOpen      = mockOpen
    hClose     = mockClose
--    hSeek      = mockSeek
--    hGet       = mockGet
--    hPut       = mockPut
--    hPutBuffer = mockPutBuffer
--    hTruncate  = mockTruncate
--    withFile   = mockWithFile
--
--    createDirectory          = mockCreateDirectory
--    createDirectoryIfMissing = mockCreateDirectoryIfMissing
--    listDirectory            = mockListDirectory
--    doesDirectoryExist       = mockDoesDirectoryExist
--    doesFileExist            = mockDoesFileExist

getFsVar :: Monad m => SimFSE m (TVar m MockFS)
getFsVar = lift $ SimFS $ ask


mockDumpState :: MonadSTM m => SimFSE m String
mockDumpState = do
    fsVar <- getFsVar
    fs <- atomically $ readTVar fsVar
    return $ prettyShowFS fs

-- | Mock implementation of 'hOpen'.
mockOpen :: CanSimFS m => FsPath -> IOMode -> m MockHandle
mockOpen fp ioMode = modifyMockFS $ \fs -> do
    -- Check if the parent exists
    parent <- noteT (FsError FsResourceDoesNotExist (Just fp) callStack) $
                mfIndex (init fp) (mockFiles fs)

    -- Check if the file exist
    (fs', _fileSize) <- case mfIndex [last fp] parent of
        Nothing | ioMode /= IO.ReadMode -> do
            let tree' = mfTouch fp (mockFiles fs)
            return (fs { mockFiles = tree' }, 0)
        Nothing ->
          throwFsError FsResourceDoesNotExist (Just fp)
        Just (FileOnDisk f) ->
          return (fs, BS.length f)
        Just (FolderOnDisk _) ->
          throwFsError FsResourceInappropriateType (Just fp)

    let initialOffset = 0
        hs            = MockHandleState fp ioMode (toEnum initialOffset)

    return (
        mockNextHandle fs'
      , fs' { mockNextHandle = succ (mockNextHandle fs') }
      )

-- | Mock implementation of 'hClose', which is trivial.
--
-- TODO: I think we should model handles that have been closed.
mockClose :: Monad m => MockHandle -> m ()
mockClose _ = return ()

-- | Mock implementation of 'hSeek'
mockSeek :: CanSimFS m => MockHandle -> SeekMode -> Word64 -> m Word64
mockSeek h mode offset = withHandleRead h $ \fs hs@MockHandleState{..} ->
    case mfIndex mhFilePath fs of
      Nothing ->
        throwFsError FsResourceDoesNotExist (Just mhFilePath)
      Just (FolderOnDisk _) ->
        throwFsError FsResourceInappropriateType (Just mhFilePath)
      Just (FileOnDisk block) -> do
        let offset' = case mode of
             IO.AbsoluteSeek -> offset
             IO.RelativeSeek -> mhOffset + offset
             IO.SeekFromEnd  -> (toEnum $ BS.length block) + offset
        -- TODO: In the initial code there was an overflow check to verify that
        -- the new offset wasn't past the end of the file, and threw an error
        -- otherwise. This got removed by Kostas since real IO does not throw
        -- such an exception. However, shouldn't we at least silently limit the
        -- offset to the end of the file?
        return (offset', hs { mhOffset = offset' })

mockGet :: CanSimFS m => MockHandle -> Int -> m ByteString
mockGet _ 0 = return BS.empty
mockGet h n = withHandleRead h $ \fs hs@MockHandleState{..} ->
    case (mhIOMode, mfIndex mhFilePath fs) of
      (AppendMode, _) ->
        throwFsError FsInvalidArgument Nothing
      (_, Nothing) ->
        throwFsError FsResourceDoesNotExist (Just mhFilePath)
      (_, Just (FolderOnDisk _)) ->
        throwFsError FsResourceInappropriateType (Just mhFilePath)
      (_, Just (FileOnDisk block)) -> do
        let r = BS.take n . BS.drop (fromEnum mhOffset) $ block
        return (r, hs { mhOffset = mhOffset + fromIntegral (BS.length r) })

mockPut :: CanSimFS m => MockHandle -> Builder -> m Word64
mockPut h builder = withHandleModify h $ \fs hs@MockHandleState{..} ->
    case (mhIOMode, mfIndex mhFilePath fs) of
      (IO.ReadMode, _) ->
        -- The IOException in IOFS gives us no FilePath in this case, so we must
        -- follow to get equivalent results (even though we actually know the fp
        -- here).
        --
        -- TODO: It might in fact be more useful to modify the IO implementation
        -- here and add the filepath there.
        throwFsError FsInvalidArgument Nothing
      (_, Nothing) ->
        throwFsError FsResourceDoesNotExist (Just mhFilePath)
      (_, Just (FolderOnDisk _)) ->
        throwFsError FsResourceInappropriateType (Just mhFilePath)
      (_, Just (FileOnDisk block)) -> do
        -- TODO: Are we sure this is correct? Surely even if we open a file in
        -- append mode, that doesn't necessarily mean we are /always/ appending?
        -- Can we not seek in append mode?
        let offsetBefore =
                if mhIOMode == AppendMode then fromIntegral $ BS.length block
                                          else mhOffset
            (unchanged, toModify) = BS.splitAt (fromEnum offsetBefore) block
            block' = unchanged <> toWrite <> BS.drop (BS.length toWrite) toModify
        return (
            bytesWritten
          , ( mfReplace mhFilePath block' fs
            , hs { mhOffset = mhOffset + bytesWritten }
            )
          )
  where
    toWrite      = BL.toStrict . toLazyByteString $ builder
    bytesWritten = toEnum $ BS.length toWrite

mockPutBuffer :: CanSimFS m => MockHandle -> buf -> Builder -> m Word64
mockPutBuffer hnd _buf builder = mockPut hnd builder

mockTruncate :: CanSimFS m => MockHandle -> Word64 -> m ()
mockTruncate h sz = withHandleModify h $ \fs hs@MockHandleState{..} ->
    case mfIndex mhFilePath fs of
      Nothing ->
        throwFsError FsResourceDoesNotExist (Just mhFilePath)
      Just (FolderOnDisk _) ->
        throwFsError FsResourceInappropriateType (Just mhFilePath)
      Just (FileOnDisk block) ->
        -- TODO: Shouldn't this modify the file handle?
        return (
            ()
          , ( mfReplace mhFilePath (BS.take (fromEnum sz) block) fs
            , hs
            )
          )

mockWithFile :: (CanSimFS m, MonadMask m)
             => FsPath -> IOMode -> (MockHandle -> m r) -> m r
mockWithFile fp ioMode = bracket (mockOpen fp ioMode) mockClose

{------------------------------------------------------------------------------
  Operations on directories
------------------------------------------------------------------------------}

mockCreateDirectoryIfMissing :: CanSimFS m => Bool -> FsPath -> m ()
mockCreateDirectoryIfMissing createParents path = modifyMockFS $ \fs -> do
    return ((), fs {
        mockFiles = mfCreateDirIfMissing createParents
                                         path
                                         (mockFiles fs)
      })

mockCreateDirectory :: CanSimFS m => FsPath -> m ()
mockCreateDirectory dir = modifyMockFS $ \fs -> do
    _parent <- noteT (FsError FsResourceDoesNotExist (Just dir) callStack) $
                   mfIndex (init dir) (mockFiles fs)
    fs'     <- noteT (FsError FsResourceAlreadyExist (Just dir) callStack) $
                   mfCreateDirStrict dir (mockFiles fs)
    return ((), fs { mockFiles = fs' })

mockListDirectory :: CanSimFS m => FsPath -> m [String]
mockListDirectory fp = readMockFS $ \fs ->
    case mfIndex fp fs of
      Nothing ->
        throwFsError FsResourceDoesNotExist (Just fp)
      Just (FileOnDisk _) ->
        throwFsError FsResourceInappropriateType (Just fp)
      Just (FolderOnDisk m) ->
        return (M.keys m)

mockDoesDirectoryExist :: CanSimFS m => FsPath -> m Bool
mockDoesDirectoryExist fp = readMockFS $ \fs -> return $
    case mfIndex fp fs of
      Just (FolderOnDisk _) -> True
      _                     -> False

mockDoesFileExist :: CanSimFS m => FsPath -> m Bool
mockDoesFileExist fp = readMockFS $ \fs -> return $
    case mfIndex fp fs of
      Just (FileOnDisk _) -> True
      _                   -> False

mockNewBuffer :: Monad m => Int -> m (Buffer (SimFSE m))
mockNewBuffer _ = return MockBufferUnused

{-------------------------------------------------------------------------------
  Auxiliary and utility functions
-------------------------------------------------------------------------------}

noteT :: MonadError e m => e -> Maybe a -> m a
noteT e = maybe (throwError e) return

-- | Renders the 'MockFS' in a human-readable fashion.
prettyShowFS :: MockFS -> String
prettyShowFS = undefined
{-
prettyShowFS MockFS{..} =
    let prettyDisk =
            map (\(fp, disk) -> "- " <> fp
                                     <> " -> "
                                     <> show (hexDump $ B16.encode disk)
                                     <> " (" <> show disk <> ")")
                (collectFiles getMockFS)
    in L.intercalate "\n" prettyDisk <> "\n\n" <> show getMockFS
    where
        collectFiles :: FsTree ByteString -> [(String, ByteString)]
        collectFiles (FileOnDisk _)   = []
        collectFiles (FolderOnDisk m) =
            foldl' (\acc x -> case x of
                   (fn, FileOnDisk c) -> (fn, c) : acc
                   (_, dir)           -> acc <> collectFiles dir
            ) [] (M.toList m)

        hexDump :: ByteString -> ByteString
        hexDump = fst
                . BS.foldl' (\(acc, n) w8 ->
                                if n == 2 then (acc <> " " <> BS.singleton w8, 1)
                                          else (acc <> BS.singleton w8, n + 1)
                            ) (mempty, 0 :: Int)
-}

demoMockFS :: MockFS
demoMockFS = undefined
{-
demoMockFS = MockFS {
    getMockFS = FolderOnDisk $ M.fromList [
        ("usr", FolderOnDisk $ M.fromList [
            ("local", FolderOnDisk $ M.fromList [
                ("bin", FolderOnDisk mempty)
            ])
        ])
      , ("var", FolderOnDisk $ M.fromList [
           ("log", FolderOnDisk mempty)
        ,  ("tmp", FolderOnDisk $ M.fromList [
             ("foo.txt", FileOnDisk mempty)
           ])
        ,  ("mail", FolderOnDisk mempty)
        ,  ("run", FolderOnDisk mempty)
      ])
    ]
    }
-}

newEmptyMockFS :: MockFS
newEmptyMockFS = undefined
{-
newEmptyMockFS = MockFS {
    getMockFS = FolderOnDisk mempty
  }
-}

-- | Runs a 'SimFs' computation provided an initial 'MockFS', producing a
-- result, the final state of the filesystem and a sequence of actions occurred
-- in the filesystem.
runSimFS :: MonadSTM m
         => SimFS m a
         -> MockFS
         -> m (a, MockFS)
runSimFS m s = do
    fs  <- atomically (newTVar s)
    a   <- runReaderT (unSimFs m) fs
    fs' <- atomically (readTVar fs)
    return (a, fs')

mockDemoScript :: (HasCallStack, HasFS m) => m [ByteString]
mockDemoScript = do
  h1 <- hOpen ["cardano.txt"] IO.ReadWriteMode
  _  <- hPut h1 (BS.byteString $ C8.pack "test")
  _  <- hSeek h1 IO.AbsoluteSeek 0
  r1 <- hGet h1 4
  _  <- hPut h1 (BS.byteString $ C8.pack "ing")
  h2 <- hOpen ["bar.txt"] IO.ReadWriteMode
  _  <- hPut h2 (BS.byteString $ C8.pack "blockchain")
  _  <- hSeek h2 IO.AbsoluteSeek 0
  r2 <- hGet h2 5
  _  <- listDirectory []
  _  <- listDirectory ["var"]
  createDirectory ["var", "tmp", "my-temp-dir"]
  createDirectoryIfMissing True ["home", "adinapoli", "test", "foo", "bar"]
  f1 <- L.sort <$> listDirectory ["var", "tmp"]
  hClose h1
  hClose h2
  checkThat "listDirectory [var, tmp]" ((==) (L.sort ["my-temp-dir", "foo.txt"])) f1
  checkThat "hGet h1 4" ((==) "test") r1
  checkThat "hGet h2 5" ((==) "block") r2
  return [r1, r2]

-- | A homemade version of 'assert' suitable for testing code which won't be
-- run in production, as unlike 'assert' this is always run regardless of the
-- optimise flag used.
checkThat :: (Show a, Monad m)
          => String
          -> (a -> Bool)
          -> a
          -> m ()
checkThat label prd a =
    if prd a then return ()
             else error $ label
                        ++ ": condition didn't hold. Actual value was "
                        ++ show a
                        ++ "\n"
                        ++ prettyCallStack callStack

mockDemo :: IO ()
mockDemo = do
    (res, fs) <- runSimFS (runExceptT demo) demoMockFS
    case res of
      Left  err -> putStrLn (prettyFSError err)
      Right bs  -> putStrLn (prettyShowFS fs) >> print bs
  where
    demo :: ExceptT FsError (SimFS IO) [ByteString]
    demo = mockDemoScript
