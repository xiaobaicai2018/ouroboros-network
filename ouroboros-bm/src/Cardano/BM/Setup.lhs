
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

import           Cardano.BM.Controller (insertInController)
import           Cardano.BM.Data (OutputKind (..), Severity (..),
                     TraceConfiguration (..), TraceContext (..),
                     TraceController (..))
import           Cardano.BM.Trace (Trace, appendName, natTrace, noTrace,
                     stdoutTrace, traceInTVarIO, traceNamedInTVarIO)
\end{code}
%endif

\begin{code}

setupTrace :: MonadIO m => TraceConfiguration -> m (Trace m)
setupTrace (TraceConfiguration outputKind name traceTransformer) = do
    ctx <- liftIO $ newContext
    let logTrace0 = case outputKind of
            StdOut             -> natTrace liftIO stdoutTrace
            TVarList      tvar -> natTrace liftIO $ traceInTVarIO tvar
            TVarListNamed tvar -> natTrace liftIO $ traceNamedInTVarIO tvar
            Null               -> noTrace

    let logTrace = (ctx, logTrace0)
    liftIO $ insertInController logTrace name traceTransformer
    return $ appendName name logTrace

withTrace :: MonadIO m =>  TraceConfiguration -> (Trace m -> m t) -> m t
withTrace cfg action = do
    logTrace <- setupTrace cfg
    action logTrace
\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}

newContext :: IO TraceContext
newContext = do
    ctrl <- newMVar $ TraceController mempty mempty Debug
    return $ TraceContext {
      loggerName = ""
    , controller = ctrl
    }

\end{code}
