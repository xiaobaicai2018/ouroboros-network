
\subsection{Cardano.BM.Setup}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Setup
    (
      setupTrace
    , withTrace
    ) where

import           Control.Concurrent.MVar (newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Map (singleton)

import           Cardano.BM.Data (LoggerName, OutputKind (..), Severity (..),
                     TraceConfiguration (..), TraceContext (..),
                     TraceController (..), TraceTransformer)
import           Cardano.BM.Trace (Trace, natTrace, noTrace, stdoutTrace,
                     traceInTVarIO, traceNamedInTVarIO, transformTrace)
\end{code}
%endif

\begin{code}

setupTrace :: MonadIO m => TraceConfiguration -> m (Trace m)
setupTrace (TraceConfiguration outputKind name traceTransformer sev) = do
    ctx <- liftIO $ newContext name traceTransformer sev
    let logTrace0 = case outputKind of
            StdOut             -> natTrace liftIO stdoutTrace
            TVarList      tvar -> natTrace liftIO $ traceInTVarIO tvar
            TVarListNamed tvar -> natTrace liftIO $ traceNamedInTVarIO tvar
            Null               -> noTrace

    let logTrace = (ctx, logTrace0)
    (_, logTrace') <- transformTrace "" logTrace
    return logTrace'

withTrace :: MonadIO m =>  TraceConfiguration -> (Trace m -> m t) -> m t
withTrace cfg action = do
    logTrace <- setupTrace cfg
    action logTrace
\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}

newContext :: LoggerName -> TraceTransformer -> Severity -> IO TraceContext
newContext name traceTransformer sev = do
    ctrl <- newMVar $ TraceController {
                          traceTransformers = singleton name traceTransformer
                        , severityMap = singleton name sev
                        , minSeverity = sev
                        }
    return $ TraceContext {
        loggerName = name
      , controller = ctrl
      }

\end{code}
