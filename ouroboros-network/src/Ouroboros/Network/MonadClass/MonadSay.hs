module Ouroboros.Network.MonadClass.MonadSay where

import           Control.Monad.IO.Class (liftIO)

class Monad m => MonadSay m where
  say :: String -> m ()
  liftIOaction :: IO a -> m a

instance MonadSay IO where
  say = print
  liftIOaction = liftIO
