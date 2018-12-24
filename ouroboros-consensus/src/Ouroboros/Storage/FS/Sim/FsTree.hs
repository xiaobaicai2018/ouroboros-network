{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal part of the mock file system
--
-- Intended for qualified import
--
-- > import Ouroboros.Storage.FS.Sim.FsTree (FsTree)
-- > import Ouroboros.Storage.FS.Sim.FsTree as FS
module Ouroboros.Storage.FS.Sim.FsTree (
    -- * FsTree type and indexing functions
    FsTree(..)
  , index
  , adjustIndex
  , adjustIndexF
    -- * File system operations
  , touch
  , replace
  , createDirIfMissing
  , createNewDir
  ) where

import           Data.Functor.Const
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Storage.FS.Class (FsPath)

{-------------------------------------------------------------------------------
  FsTree type and general indexing functions
-------------------------------------------------------------------------------}

-- | A simple representation of files and folders on disk, where leaves stores
-- not just the file names but also the actual content of these files.
data FsTree a = File a | Folder (Folder a)
  deriving (Show, Eq, Generic)

type Folder a = Map String (FsTree a)

-- | Most general indexing function
alterF :: forall f a. Functor f
       => FsPath
       -- ^ Path to look for
       -> (Either FsPath (FsTree a) -> f (FsTree a))
       -- ^ Function to alter or create a new tree
       -- If no tree was found, suffix of the path that was missing is returned
       -> FsTree a -> f (FsTree a)
alterF []     f t          = f (Right t)
alterF (p:ps) f (File   _) = error "alterF: invalid path"
alterF (p:ps) f (Folder m) = Folder <$> M.alterF (fmap Just . go) p m
  where
    go :: Maybe (FsTree a) -> f (FsTree a)
    go Nothing  = f (Left (p:ps))
    go (Just t) = alterF ps f t


-- | Index the FsTree by the given FsPath.
index :: FsPath -> FsTree a -> Maybe (FsTree a)
index fp = getConst . alterF fp (Const . go)
  where
    go :: Either FsPath (FsTree a) -> Maybe (FsTree a)
    go (Left  _) = Nothing
    go (Right t) = Just t

-- | Traverses a 'FsTree' indexing by the given 'FsPath' and, if a match is
-- found, apply the input action on the tree node.
adjustIndex :: FsPath
            -> (FsTree a -> FsTree a)
            -> (FsTree a -> FsTree a)
adjustIndex p f = runIdentity . adjustIndexF p (Identity . f)

-- | General version of 'adjustIndex' parametric over an applicative functor f.
adjustIndexF :: forall f a. Applicative f
             => FsPath
             -> (FsTree a -> f (FsTree a))
             -> (FsTree a -> f (FsTree a))
adjustIndexF []     f t          = f t
adjustIndexF (_:_)  _ (File   _) = error "adjustIndexF: invalid path"
adjustIndexF (p:ps) f (Folder m) = Folder <$> M.alterF go p m
  where
    go :: Maybe (FsTree a) -> f (Maybe (FsTree a))
    go Nothing   = pure Nothing
    go (Just ts) = Just <$> adjustIndexF ps f ts


{-------------------------------------------------------------------------------
  Alterating files and directories
-------------------------------------------------------------------------------}

alterDir :: forall f a. Functor f
         => FsPath
         -> (Folder a -> f (Folder a))
         -> (FsTree a -> f (FsTree a))
alterDir p f = alterF p go
  where
    go :: Either FsPath (FsTree a) -> f (FsTree a)
    go (Left  [dir])      = Folder <$> f M.empty
    go (Right (Folder m)) = Folder <$> f m
    go (Left  (dir:_))    = error $ "alterDir: " ++ show dir ++ " missing"
    go (Right (File   _)) = error $ "alterDir: invalid path"

alterFile :: forall f a. (Functor f, Monoid a)
          => FsPath
          -> (a -> f a)
          -> (FsTree a -> f (FsTree a))
alterFile p f = alterF p go
  where
    go :: Either FsPath (FsTree a) -> f (FsTree a)
    go (Left  [file])     = File <$> f mempty
    go (Right (File a))   = File <$> f a
    go (Left  (dir:_))    = error $ "alterFile: " ++ show dir ++ " missing"
    go (Right (Folder m)) = error $ "alterFile: invalid path"

{-------------------------------------------------------------------------------
  Specific file system functions
-------------------------------------------------------------------------------}

touch :: (HasCallStack, Monoid a) => FsPath -> FsTree a -> FsTree a
touch fp = runIdentity . alterFile fp pure

replace :: HasCallStack => FsPath -> a -> FsTree a -> FsTree a
replace = _

{-
replace :: HasCallStack => FsPath -> a -> FsTree a -> FsTree a
replace fp content =
    adjustIndex fp $ \case
      FileOnDisk _ -> FileOnDisk content
      _otherwise   ->
        error $ "replace: found a folder, not a file for fp " <> show fp

createDirIfMissing :: Bool -> FsPath -> FsTree a -> FsTree a
createDirIfMissing createParents path0 files
    | createParents =
        -- returns the parents paths, but not the empty list [].
        -- >    inits ["a", "b", "c"]
        -- > == [[], ["a"], ["a", "b"], ["a", "b", "c"]]
        -- but we do want only the last 3 elements.
        let parents = tail (L.inits path0)
        in foldl (flip createDir) files parents
    | otherwise = createDir path0 files

-- | Creates a directory (does not throw an error if it exists already)
createDir :: HasCallStack => FsPath -> FsTree a -> FsTree a
createDir fp =
    adjustIndex (init fp) $ \case
      FolderOnDisk m -> newDir m
      _otherwise ->
        error $ "createDir: found file, not folder for fp " <> show fp
  where
    newDir m =
        case M.lookup (last fp) m of
          Nothing -> FolderOnDisk $ M.insert (last fp) (FolderOnDisk mempty) m
          Just _  -> FolderOnDisk m

createNewDir :: HasCallStack => FsPath -> FsTree a -> Maybe (FsTree a)
createNewDir fp =
    adjustIndexF (init fp) $ \case
      FolderOnDisk m -> newDir m
      _otherwise ->
        error $ "createNewDir: found file, not folder for fp " <> show fp
  where
    newDir :: Map String (FsTree a) -> Maybe (FsTree a)
    newDir m =
        case M.lookup (last fp) m of
          Nothing -> Just $ FolderOnDisk $ M.insert (last fp) (FolderOnDisk mempty) m
          Just _  -> Nothing
-}
