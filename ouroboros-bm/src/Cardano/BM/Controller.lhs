
\subsection{Cardano.BM.Controller}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Controller
    (
      findTraceTransformer
    , insertInController
    , setMinSeverity
    , setNamedSeverity
    , getNamedSeverity
    , checkSeverity
    , appendWithDot
    ) where

import           Prelude hiding (lookup, take)


import           Control.Concurrent.MVar (modifyMVar_, withMVar)
import           Data.Map (findWithDefault, insert, lookup)
import           Data.Text (Text, take)

import           Cardano.BM.Data (LogItem (..), LoggerName, Severity (..),
                     Trace, TraceContext (..), TraceController (..),
                     SubTrace (..))

\end{code}
%endif

\subsubsection{Add a |SubTrace| to this |Trace|}\label{code:insertInController}
It will get activated once we enter the named context.
\begin{code}
insertInController :: Monad m =>  Trace m -> Text -> SubTrace -> IO ()
insertInController (ctx, _) name trans = do
    let currentLoggerName = loggerName ctx
        name' = appendWithDot currentLoggerName name
    modifyMVar_ (controller ctx) $ \tc ->
        return $ tc { traceTransformers = insert name' trans (traceTransformers tc) }

\end{code}

\begin{code}
setMinSeverity :: Trace m -> Severity -> IO ()
setMinSeverity (ctx, _) newMinSeverity =
    modifyMVar_ (controller ctx) $ \tc ->
        return $ tc { minSeverity = newMinSeverity }

\end{code}

\begin{code}
setNamedSeverity :: TraceContext -> LoggerName -> Severity -> IO ()
setNamedSeverity ctx name newSeverity =
    modifyMVar_ (controller ctx) $ \tc ->
        return $ tc { severityMap = insert name newSeverity (severityMap tc) }

\end{code}

\begin{code}
getNamedSeverity :: TraceContext -> LoggerName -> IO (Maybe Severity)
getNamedSeverity ctx name = withMVar (controller ctx) $ \tc ->
    return $ lookup name (severityMap tc)

findTraceTransformer :: Trace m -> Text -> IO SubTrace
findTraceTransformer (ctx, _) name =
    withMVar (controller ctx) $ \tc ->
        return $ findWithDefault Neutral name (traceTransformers tc)

appendWithDot :: LoggerName -> LoggerName -> LoggerName
appendWithDot "" newName = take 50 newName
appendWithDot xs ""      = xs
appendWithDot xs newName = take 50 $ xs <> "." <> newName

\end{code}

\subsubsection{checkSeverity}\label{code:checkSeverity}
\begin{code}
checkSeverity :: TraceContext -> LogItem -> IO Bool
checkSeverity ctx item = do
    let name = loggerName ctx
        itemSev = liSeverity item
    withMVar (controller ctx) $ \tc -> do
        let globalSev = minSeverity tc
        case lookup name (severityMap tc) of
            Nothing          -> return (itemSev >= globalSev)
            Just specificSev -> return ((itemSev >= globalSev) && (itemSev >= specificSev))
\end{code}
