
\subsection{Cardano.BM.Controller}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Controller
    (
      findTraceTransformer
    , insertInController
    , setMinSeverity
    , checkSeverity
    ) where

import           Control.Concurrent.MVar (modifyMVar_, withMVar)

import           Data.Map (findWithDefault, insert)
import           Data.Text (Text)

import           Cardano.BM.Data (LogItem (..), Severity (..), Trace,
                     TraceContext (..), TraceController (..),
                     TraceTransformer (..))

\end{code}
%endif

\begin{code}

findTraceTransformer :: Trace m -> Text -> IO TraceTransformer
findTraceTransformer (ctx, _) name =
    withMVar (controller ctx) $ \tc ->
        return $ findWithDefault Neutral name (traceTransformers tc)

insertInController :: Monad m =>  Trace m -> Text -> TraceTransformer -> IO ()
insertInController (ctx, _) name trans =
    modifyMVar_ (controller ctx) $ \tc ->
        return $ tc { traceTransformers = insert name trans (traceTransformers tc) }

setMinSeverity :: Trace m -> Severity -> IO ()
setMinSeverity (ctx, _) newMinSeverity =
    modifyMVar_ (controller ctx) $ \tc ->
        return $ tc { minSeverity = newMinSeverity }

\end{code}

\subsubsection{checkSeverity}\label{code:checkSeverity}
\begin{code}
checkSeverity :: TraceContext -> LogItem -> IO Bool
checkSeverity ctx item =
    withMVar (controller ctx) $ \tc ->
        return (liSeverity item >= minSeverity tc)
\end{code}

