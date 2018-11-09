
\subsection{STM}

\begin{code}
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

import           Cardano.BM.Trace (Trace, appendName, logDebug, logInfo)
\end{code}


\begin{code}
nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{code}

%if False
\begin{code}
{-# NOINLINE measure_atomically #-}
\end{code}
%endif

\begin{code}
measure_atomically :: (MonadIO m) => Trace m -> Text -> STM.STM a -> m a
measure_atomically logTrace0 name stm = do
    let logTrace = appendName name logTrace0
    logDebug logTrace $ "entering " <> name
    tstart <- liftIO getMonotonicTimeNSec
    res <- liftIO $ STM.atomically stm
    tend <- liftIO getMonotonicTimeNSec
    logDebug logTrace $ "leaving " <> name
    let tdiff = nominalDiffTimeToMicroseconds (tend - tstart)
    logInfo logTrace $ "eval of " <> name <> " took " <> t(show tdiff)
    return res

  where
    t = pack

\end{code}
