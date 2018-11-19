
\subsection{Cardano.BM.STMObserver}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.STMObserver
    (
      bracketObserveIO
    , bracketObserveLogIO
    -- * to be removed
    , measure_atomically
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM

import           Data.Monoid ((<>))
import           Data.Text
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Word (Word64)

import           Cardano.BM.Data (LogObject (..), TraceTransformer (..))
import           Cardano.BM.MonadicObserver (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, appendName, logDebug, logInfo,
                     transformTrace)
\end{code}
%endif

\begin{code}
nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{code}

%if False
\begin{code}
{-# NOINLINE measure_atomically #-}
\end{code}
%endif

\todo[inline]{remove the function |measure_atomically| which only serves some preliminary testing}
\begin{code}
measure_atomically :: (MonadIO m) => Trace m -> Text -> STM.STM a -> m a
measure_atomically logTrace0 name stm = do
    logTrace <- appendName name logTrace0
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

\begin{code}

stmWithLog :: STM.STM (t, [LogObject]) -> STM.STM (t, [LogObject])
stmWithLog action = action

\end{code}

\begin{code}

bracketObserveIO :: Trace IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    bracketObserveIO' traceTransformer logTrace action

bracketObserveIO' :: TraceTransformer -> Trace IO -> STM.STM t -> IO t
bracketObserveIO' NoTrace _ action =
    STM.atomically action
bracketObserveIO' traceTransformer logTrace action = do
    countersid <- observeOpen traceTransformer logTrace
    -- run action, returns result only
    t <- STM.atomically action
    observeClose traceTransformer logTrace countersid []
    pure t

\end{code}

\begin{code}

bracketObserveLogIO :: Trace IO -> Text -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    bracketObserveLogIO' traceTransformer logTrace action

bracketObserveLogIO' :: TraceTransformer -> Trace IO -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO' NoTrace _ action = do
    (t, _) <- STM.atomically $ stmWithLog action
    pure t
bracketObserveLogIO' traceTransformer logTrace action = do
    countersid <- observeOpen traceTransformer logTrace
    -- run action, return result and log items
    (t, as) <- STM.atomically $ stmWithLog action
    observeClose traceTransformer logTrace countersid as
    pure t

\end{code}
