
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
    ) where

import           Control.Concurrent.MVar (modifyMVar_, newMVar, takeMVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Bool (bool)
import           Data.Functor.Contravariant (Op (..))
import           Data.Map (findWithDefault, insert)
import           Data.Text (Text)

import           Cardano.BM.BaseTrace
import           Cardano.BM.Data (LogNamed (..), LogObject (..), OutputKind (..),
                     TraceConfiguration (..), TraceContext, TraceController (..),
                     TraceTransformer (..), TraceTransformerMap)
import           Cardano.BM.Trace (Trace, appendName, natTrace, noTrace,
                     stdoutTrace, traceInTVarIO, traceNamedObject)
\end{code}
%endif

\begin{code}

setupTrace :: MonadIO m => TraceConfiguration -> m (Trace m)
setupTrace (TraceConfiguration outputKind name traceTransformer) = do
    ctx <- liftIO $ newContext
    let logTrace0 = bool noTrace (natTrace liftIO stdoutTrace) (outputKind == StdOut)
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
newContext =
    newMVar $ TraceController $ mempty

getTraceContext :: TraceContext -> IO (TraceTransformerMap)
getTraceContext ctx = traceTransformers <$> takeMVar ctx

findTraceTransformer :: TraceContext -> Text -> IO TraceTransformer
findTraceTransformer ctx name = do
    transformers <- getTraceContext ctx
    return $ findWithDefault Neutral name transformers

insertInController :: Monad m =>  Trace m -> Text -> TraceTransformer -> IO ()
insertInController (ctx, _) name trans =
    modifyMVar_ ctx (\(TraceController mapping) -> return $ TraceController $ insert name trans mapping)

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
        ListTrace tvar    -> (traceTransformer, (ctx, traceInTVarIO tvar))
        ObservableTrace _ -> (traceTransformer, appendName name tr)

\end{code}

