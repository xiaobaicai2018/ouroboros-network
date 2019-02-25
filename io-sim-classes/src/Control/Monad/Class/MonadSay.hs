module Control.Monad.Class.MonadSay
  ( MonadSay (..)
  ) where

import qualified Data.ByteString.Char8 as BSC

import           Control.Monad.State

-- TODO: remove MonadSay, in favour of iohk-monitoring-framework
class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = BSC.putStr . BSC.pack . show

instance MonadSay m => MonadSay (StateT s m) where
  say = lift . say
