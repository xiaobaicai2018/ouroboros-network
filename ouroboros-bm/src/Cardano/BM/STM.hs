{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.STM
    (
      measure_atomically
    ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Text
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           Cardano.BM.Trace (TraceNamed, appendName, logDebug, logInfo,
                     logObservable)

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

{-# NOINLINE measure_atomically #-}
-- atomically :: STM a -> IO a
measure_atomically :: (MonadIO m) => STM.TVar (TraceNamed m) -> Text -> STM.STM a -> m a
measure_atomically logTrace0 name stm = do
    logTrace1 <- liftIO $ STM.atomically $ STM.readTVar logTrace0
    let logTrace = appendName name logTrace1
    logDebug logTrace $ "entering " <> name
    tstart <- liftIO getPOSIXTime
    res <- liftIO $ STM.atomically stm
    tend <- liftIO getPOSIXTime
    logDebug logTrace $ "leaving " <> name
    let tdiff = nominalDiffTimeToMicroseconds (tend - tstart)
    liftIO $ STM.atomically $ STM.modifyTVar logTrace0 (\ltr -> logObservable ltr name (fromIntegral tdiff))
    logTrace' <- liftIO $ STM.atomically $ STM.readTVar logTrace0
    logInfo logTrace' $ "eval of " <> name <> " took " <> l(show tdiff)
    return res

  where
    l = pack


