
\subsection{Cardano.BM.MonadicObserver}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.MonadicObserver
    (
      bracketObserveIO
      -- * observing functions
    , observeOpen
    , observeClose
    ) where

import           Control.Monad (forM_)

import           Data.Monoid ((<>))
import           Data.Text
import           Data.Unique (hashUnique, newUnique)


import           Cardano.BM.Data (CounterState (..), LogObject (..),
                     TraceTransformer (..))
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Trace (Trace, logInfo, traceNamedObject,
                     transformTrace)
\end{code}
%endif

\begin{code}

--   Observes an action and adds name given in the logger
--   name of the given |Trace|. If the empty |Text| is
--   given as name then the logger name remains untouched.
bracketObserveIO :: Trace IO -> Text -> IO t -> IO t
bracketObserveIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    bracketObserveIO' traceTransformer logTrace action

bracketObserveIO' :: TraceTransformer -> Trace IO -> IO t -> IO t
bracketObserveIO' NoTrace _ action = action
bracketObserveIO' traceTransformer logTrace action = do
    countersid <- observeOpen traceTransformer logTrace
    -- run action
    t <- action
    observeClose traceTransformer logTrace countersid []
    pure t

\end{code}

\begin{code}

observeOpen :: TraceTransformer -> Trace IO -> IO CounterState
observeOpen traceTransformer logTrace = do
    identifier <- newUnique
    logInfo logTrace $ "Opening: " <> pack (show $ hashUnique identifier)

    -- take measurement
    counters <- readCounters traceTransformer
    let state = CounterState identifier counters
    -- send opening message to Trace
    traceNamedObject logTrace $ ObserveOpen state
    return state

\end{code}

\begin{code}

observeClose :: TraceTransformer -> Trace IO -> CounterState -> [LogObject] -> IO ()
observeClose traceTransformer logTrace (CounterState identifier _) logObjects = do
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)

    -- take measurement
    counters <- readCounters traceTransformer
    -- send closing message to Trace
    traceNamedObject logTrace $ ObserveClose (CounterState identifier counters)
    -- trace the messages gathered from inside the action
    forM_ logObjects $ traceNamedObject logTrace

\end{code}
