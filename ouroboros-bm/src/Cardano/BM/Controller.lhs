
\subsection{Cardano.BM.Controller}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Controller
    (
      setupTrace
    , withTrace
    , transformTrace
    , mapName2TTf
    , insertInController
    , changeMinSeverity
    ) where

import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Functor.Contravariant (Op (..))
import           Data.Map (findWithDefault, insert)
import           Data.Text (Text)

import           Cardano.BM.BaseTrace
import           Cardano.BM.Data (LogNamed (..), LogObject (..),
                     OutputKind (..), Severity (..), TraceConfiguration (..),
                     TraceContext (..), TraceController (..),
                     TraceTransformer (..), TraceTransformerMap)
import           Cardano.BM.Trace (Trace, appendName, natTrace, noTrace,
                     stdoutTrace, traceInTVarIO, traceNamedInTVarIO,
                     traceNamedObject)
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

\todo[inline]{search for a better name}
\begin{code}
mapName2TTf :: Trace m -> Text -> IO TraceTransformer
mapName2TTf (ctx, _) name = findTraceTransformer ctx name

\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}

newContext :: IO TraceContext
newContext = do
    ctrl <- newMVar $ (mempty :: TraceController)
    return $ TraceContext {
      loggerName = ""
    , controller = ctrl
    }

getTraceContext :: TraceContext -> IO (TraceTransformerMap)
getTraceContext ctx = withMVar (controller ctx) $ return . traceTransformers

findTraceTransformer :: TraceContext -> Text -> IO TraceTransformer
findTraceTransformer ctx name = do
    transformers <- getTraceContext ctx
    return $ findWithDefault Neutral name transformers

insertInController :: Monad m =>  Trace m -> Text -> TraceTransformer -> IO ()
insertInController (ctx, _) name trans = do
    modifyMVar_ (controller ctx) (\tc ->
        return $ tc {
                     traceTransformers = insert name trans (traceTransformers tc) } )

changeMinSeverity :: Trace m -> Severity -> IO ()
changeMinSeverity (ctx, _) newMinSeverity = do
    modifyMVar_ (controller ctx) (\tc ->
        return $ tc { minSeverity = newMinSeverity })

\end{code}

\subsubsection{transformTrace}\label{code:transformTrace}
\begin{code}

transformTrace :: Text -> Trace IO -> IO (TraceTransformer, Trace IO)
transformTrace name tr@(ctx, _) = do
    traceTransformer <- mapName2TTf tr name
    return $ case traceTransformer of
        Neutral      -> (traceTransformer, appendName name tr)
        UntimedTrace -> (traceTransformer, appendName name tr)
        NoTrace      -> (traceTransformer, (ctx, BaseTrace $ Op $ \_ -> pure ()))
        DropOpening  -> (traceTransformer, (ctx, BaseTrace $ Op $ \lognamed ->
            case lnItem lognamed of
                ObserveOpen _ -> return ()
                obj           -> traceNamedObject tr obj))
        ObservableTrace _ -> (traceTransformer, appendName name tr)

\end{code}
