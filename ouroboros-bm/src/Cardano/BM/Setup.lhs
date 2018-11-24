
\subsection{Cardano.BM.Setup}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Setup
    (
      setupTrace
    , withTrace
    , setupComponents
    ) where

import           Control.Concurrent.MVar (newMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Map (singleton)

import qualified Cardano.BM.Configuration
import           Cardano.BM.Data (LoggerName, OutputKind (..), Severity (..),
                     TraceConfiguration (..), TraceContext (..),
                     TraceController (..), TraceTransformer)
import qualified Cardano.BM.Output.Switchboard
import qualified Cardano.BM.Output.Aggregation
import qualified Cardano.BM.Output.EKGView
import qualified Cardano.BM.Output.Katip
import           Cardano.BM.Trace (Trace, natTrace, noTrace, stdoutTrace,
                     traceInTVarIO, traceNamedInTVarIO, transformTrace)

\end{code}
%endif

\subsubsection{setupTrace}\label{code:setupTrace}
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

\end{code}

\subsubsection{withTrace}\label{code:withTrace}
\begin{code}
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

\subsubsection{setupComponents}\label{code:setupComponents}
\begin{code}
setupComponents :: IO ()
setupComponents = do
    c <- Cardano.BM.Configuration.setup "some_file_path.yaml"
    Cardano.BM.Output.Switchboard.setup c
    Cardano.BM.Output.EKGView.setup c
    Cardano.BM.Output.Aggregation.setup c
    Cardano.BM.Output.Katip.setup c

\end{code}

