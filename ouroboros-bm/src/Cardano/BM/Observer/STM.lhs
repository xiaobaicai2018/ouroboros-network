
\subsection{Cardano.BM.Observer.STM}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Observer.STM
    (
      bracketObserveIO
    , bracketObserveLogIO
    ) where

import qualified Control.Monad.STM as STM

import           Data.Text

import           Cardano.BM.Data (LogObject (..), TraceTransformer (..))
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, transformTrace)
\end{code}
%endif

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
