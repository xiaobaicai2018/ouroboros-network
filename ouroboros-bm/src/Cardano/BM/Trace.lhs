
\subsection{Cardano.BM.Trace}

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      Trace
    , stdoutTrace
    , noTrace
    , traceInTVar
    , traceInTVarIO
    , traceNamedInTVarIO
    -- * context naming
    , appendName
    -- * utils
    , natTrace
    -- * log functions
    , traceNamedObject
    , logDebug,   logDebugS,   logDebugP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningUnsafeP

    ) where


import           Control.Concurrent.MVar (MVar, newMVar, withMVar)

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Data.Aeson.Text (encodeToLazyText)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)

import           Cardano.BM.BaseTrace
import           Cardano.BM.Data

import           System.IO.Unsafe (unsafePerformIO)
\end{code}
%endif

\begin{code}
-- add/modify named context
modifyName
    :: ([ContextName] -> [ContextName])
    -> TraceNamed m
    -> TraceNamed m
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: Text -> Trace m -> Trace m
appendName lname (c,ltr) = (c, modifyName (\e -> [lname] <> e) ltr)

-- return a BaseTrace from a TraceNamed
named :: BaseTrace m (LogNamed i) -> BaseTrace m i
named = contramap (LogNamed mempty)

\end{code}

\todo[inline]{TODO remove |locallock|}
%if False
\begin{code}
{-# NOINLINE locallock #-}
\end{code}
%endif
\begin{code}
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()
\end{code}

\subsubsection{Concrete Trace on stdout}\label{code:stdoutTrace}

This function returns a trace with an action of type "|(LogNamed LogObject) -> IO ()|"
which will output a text message as text and all others as JSON encoded representation
to the console.

\begin{code}
stdoutTrace :: TraceNamed IO
stdoutTrace = BaseTrace $ Op $ \lognamed ->
    case lnItem lognamed of
        LP (LogMessage logItem) ->
            withMVar locallock $ \_ ->
                output (lnName lognamed) $ liPayload logItem
        obj ->
            withMVar locallock $ \_ ->
                output (lnName lognamed) $ toStrict (encodeToLazyText obj)
  where
    output nm msg = TIO.putStrLn $ contextname nm <> " :: " <> msg
    contextname :: [ContextName] -> Text
    contextname (y : ys) = foldl (\e a -> e <> "." <> a) y ys
    contextname []       = "(null name)"

\end{code}

\subsubsection{Concrete Trace into a |TVar|}\label{code:traceInTVar}\label{code:traceInTVarIO}

\begin{code}

traceInTVar :: STM.TVar [a] -> BaseTrace STM.STM a
traceInTVar tvar = BaseTrace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) (lnItem ln))

traceNamedInTVarIO :: STM.TVar [LogNamed LogObject] -> TraceNamed IO
traceNamedInTVarIO tvar = BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) ln)
\end{code}

\subsubsection{Enter message into a trace}\label{code:traceNamedItem}

The function |traceNamedItem| creates a |LogObject| and threads this through
the action defined in the |Trace|.

\begin{code}

traceNamedItem
    :: Trace m
    -> LogSelection
    -> Severity
    -> Text
    -> m ()
traceNamedItem (_, logTrace) p s m =
    traceWith (named logTrace) $ LP $ LogMessage $ LogItem { liSelection = p
                                                           , liSeverity  = s
                                                           , liPayload   = m
                                                           }

{-
logMessage, logMessageS, logMessageP :: Trace m -> Severity -> Text -> m ()
logMessage logTrace  = traceNamedItem logTrace Both
logMessageS logTrace = traceNamedItem logTrace Private
logMessageP logTrace = traceNamedItem logTrace Public
-}

logDebug, logInfo, logNotice, logWarning, logError
    :: Trace m -> Text -> m ()
logDebug logTrace   = traceNamedItem logTrace Both Debug
logInfo logTrace    = traceNamedItem logTrace Both Info
logNotice logTrace  = traceNamedItem logTrace Both Notice
logWarning logTrace = traceNamedItem logTrace Both Warning
logError logTrace   = traceNamedItem logTrace Both Error

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: Trace m -> Text -> m ()
logDebugS logTrace   = traceNamedItem logTrace Private Debug
logInfoS logTrace    = traceNamedItem logTrace Private Info
logNoticeS logTrace  = traceNamedItem logTrace Private Notice
logWarningS logTrace = traceNamedItem logTrace Private Warning
logErrorS logTrace   = traceNamedItem logTrace Private Error

logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: Trace m -> Text -> m ()
logDebugP logTrace   = traceNamedItem logTrace Public Debug
logInfoP logTrace    = traceNamedItem logTrace Public Info
logNoticeP logTrace  = traceNamedItem logTrace Public Notice
logWarningP logTrace = traceNamedItem logTrace Public Warning
logErrorP logTrace   = traceNamedItem logTrace Public Error

logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: Trace m -> Text -> m ()
logDebugUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Debug
logInfoUnsafeP logTrace    = traceNamedItem logTrace PublicUnsafe Info
logNoticeUnsafeP logTrace  = traceNamedItem logTrace PublicUnsafe Notice
logWarningUnsafeP logTrace = traceNamedItem logTrace PublicUnsafe Warning
logErrorUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Error

\end{code}

%if False
\begin{spec}

example :: IO ()
example = do
    let logTrace0 = stdoutTrace
    ctx <- newMVar $ TraceController $ mempty
    let logTrace = appendName "my_example" (ctx, logTrace0)
    insertInOracle logTrace "expect_answer" Neutral
    result <- bracketObserveIO logTrace "expect_answer" setVar\_
    logInfo logTrace $ pack $ show result

example\_TVar :: IO ()
example\_TVar = do
    tvar <- STM.newTVarIO []
    let logTrace0 = traceInTVarIO tvar
    ctx <- newMVar $ TraceController $ mempty
    let logTrace = appendName "my_example" $ (ctx, logTrace0)
    result <- bracketObserveIO logTrace "expect_answer" setVar_
    logInfo logTrace $ pack $ show result
    items <- STM.readTVarIO tvar
    TIO.putStrLn $ pack $ show $ dropPrims $ items
  where
    dropPrims :: [LogObject] -> [LogObject]
    dropPrims = filter (\case {LP _ -> False; _ -> True})

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

exampleConfiguration :: IO Integer
exampleConfiguration = withTrace (TraceConfiguration StdOut "my_example" (ObservableTrace observablesSet)) $
    \tr -> bracketObserveIO tr "my_example" setVar\_
  where
    observablesSet :: Set ObservableInstance
    observablesSet = fromList [MonotonicClock, MemoryStats]

\end{spec}
%endif

\begin{code}

traceNamedObject
    :: Trace m
    -> LogObject
    -> m ()
traceNamedObject (_, logTrace) = traceWith (named logTrace)

\end{code}
