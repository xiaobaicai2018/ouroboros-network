{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.STM
    (
      measure_atomically
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM

import           Data.Monoid ((<>))
import           Data.Text
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Word (Word64)

import           Cardano.BM.Trace (TraceNamedE, appendName, logDebug, logInfo)


nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)

{-# NOINLINE measure_atomically #-}
-- atomically :: STM a -> IO a
measure_atomically :: (MonadIO m) => TraceNamedE m -> Text -> STM.STM a -> m a
measure_atomically logTrace0 name stm = do
    let logTrace = appendName name logTrace0
    logDebug logTrace $ "entering " <> name
    tstart <- liftIO getMonotonicTimeNSec
    res <- liftIO $ STM.atomically stm
    tend <- liftIO getMonotonicTimeNSec
    logDebug logTrace $ "leaving " <> name
    let tdiff = nominalDiffTimeToMicroseconds (tend - tstart)
    logInfo logTrace $ "eval of " <> name <> " took " <> t_(show tdiff)
    return res

  where
    t_ = pack


