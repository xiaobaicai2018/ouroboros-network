
\subsection{Trace}

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
    , setupTrace
    , stdoutTrace
    , noTrace
    , emptyContext
    -- * context naming
    , appendName
    -- * utils
    , natTrace
    -- * log functions
    , logDebug,   logDebugS,   logDebugP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningUnsafeP

    ) where


import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar,
                     withMVar)

import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Bool (bool)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Foldable (foldrM)
import           Data.Map (Map, findWithDefault, insert)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set, fromList, member)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds, toMicroseconds)
import           Data.Unique (Unique, hashUnique, newUnique)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Generics (Generic)
import           GHC.Word (Word64)

import           Cardano.BM.Aggregation
import           Cardano.BM.BaseTrace
import           Cardano.BM.Data

import           System.IO.Unsafe (unsafePerformIO)
\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}
A |Trace| consists of a \nameref{code:TraceContext} and a \nameref{code:TraceNamed} in |m|.
\begin{code}

type Trace m = (TraceContext, TraceNamed m)
\end{code}

\subsubsection{TraceNamed}\label{code:TraceNamed}
A |TraceNamed| is a trace of type \nameref{code:LogNamed} with payload \nameref{code:LogObject}.
\begin{code}

type TraceNamed m = BaseTrace m (LogNamed LogObject)
\end{code}

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

traceInTVar :: STM.TVar [LogObject] -> BaseTrace STM.STM LogObject
traceInTVar tvar = BaseTrace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = BaseTrace $ Op $ \lognamed -> STM.atomically $ STM.modifyTVar tvar ((:) (lnItem lognamed))
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

logMessage, logMessageS, logMessageP :: Trace m -> Severity -> Text -> m ()
logMessage logTrace  = traceNamedItem logTrace Both
logMessageS logTrace = traceNamedItem logTrace Private
logMessageP logTrace = traceNamedItem logTrace Public

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

\begin{spec}

example :: IO ()
example = do
    let logTrace0 = stdoutTrace
    ctx <- newMVar $ TraceController $ mempty
    let logTrace = appendName "my_example" (ctx, logTrace0)
    insertInOracle logTrace "expect_answer" Neutral
    result <- bracketObserveIO logTrace "expect_answer" setVar_
    logInfo logTrace $ pack $ show result

example_TVar :: IO ()
example_TVar = do
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
    \tr -> bracketObserveIO tr "my_example" setVar_
  where
    observablesSet :: Set ObservableInstance
    observablesSet = fromList [MonotonicClock, MemoryStats]

\end{spec}

\begin{code}

setupTrace :: MonadIO m => TraceConfiguration -> m (Trace m)
setupTrace (TraceConfiguration outputKind name traceTransformer) = do
    ctx <- liftIO $ emptyContext
    let logTrace0 = bool noTrace (natTrace liftIO stdoutTrace) (outputKind == StdOut)
    let logTrace = (ctx, logTrace0)
    liftIO $ insertInOracle logTrace name traceTransformer
    return $ appendName name logTrace

withTrace :: MonadIO m =>  TraceConfiguration -> (Trace m -> m t) -> m t
withTrace cfg action = do
    logTrace <- setupTrace cfg
    action logTrace
\end{code}

\begin{code}

readCounters :: TraceTransformer -> IO [Counter]
readCounters NoTrace      = return []
readCounters Neutral      = return []
readCounters UntimedTrace = return []
readCounters DropOpening  = return []
readCounters (ObservableTrace tts) = foldrM (\(sel, fun) a ->
    if sel `member` tts
    then (fun >>= \xs -> return $ a ++ xs)
    else return a) [] selectors
  where
    selectors = [(MonotonicClock, getMonoClock), (MemoryStats, readMemStats){-, (CPUTimeStats, readCPUTimeStats)-}]
    getMonoClock :: IO [Counter]
    getMonoClock = do
        t <- getMonotonicTimeNSec
        let meas = MonotonicClockTime "test" $ nominalDiffTimeToMicroseconds t
        return $ [meas]
    readMemStats :: IO [Counter]
    readMemStats = return [MemoryResidency "one" (-1), MemoryResidency "two" (-2)]

\end{code}

\begin{code}

traceNamedObject
    :: Trace m
    -> LogObject
    -> m ()
traceNamedObject (_, logTrace) = traceWith (named logTrace)

\end{code}

\begin{code}

stmWithLog :: STM.STM (t, [LogObject]) -> STM.STM (t, [LogObject])
stmWithLog action = action

\end{code}

\begin{code}

bracketObserveIO :: Trace IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    countersid <- observeOpen traceTransformer logTrace
    -- run action, returns result only
    t <- STM.atomically action
    observeClose traceTransformer logTrace countersid []
    pure t

bracketObserveLogIO :: Trace IO -> Text -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    countersid <- observeOpen traceTransformer logTrace
    -- run action, return result and log items
    (t, as) <- STM.atomically $ stmWithLog action
    observeClose traceTransformer logTrace countersid as
    pure t

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

observeClose :: TraceTransformer -> Trace IO -> CounterState -> [LogObject] -> IO ()
observeClose traceTransformer logTrace counterState logObjects = do
    let identifier = csIdentifier counterState
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)
    let msgs = filterPrims logObjects

    -- take measurement
    counters <- readCounters traceTransformer
    let state = CounterState identifier counters
    -- send closing message to Trace
    traceNamedObject logTrace $ ObserveClose state msgs
    -- trace the messages gathered from inside the action
    forM_ msgs $ traceNamedObject logTrace . LP
  where
    filterPrims :: [LogObject] -> [LogPrims]
    filterPrims = mapMaybe (\case
                                (LP a) -> Just a
                                _      -> Nothing)

\end{code}

\begin{code}

nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)

\end{code}

\begin{code}
oracle :: Trace m -> Text -> IO TraceTransformer
oracle (ctx, _) = getTraceTransformer ctx

\end{code}

\begin{code}

emptyContext :: IO TraceContext
emptyContext =
    newMVar $ TraceController $ mempty

getTraceContext :: TraceContext -> IO (Map Text TraceTransformer)
getTraceContext ctx = traceTransformers <$> takeMVar ctx

getTraceTransformer :: TraceContext -> Text -> IO TraceTransformer
getTraceTransformer ctx name = do
    transformers <- getTraceContext ctx
    return $ findWithDefault Neutral name transformers

insertInOracle :: Monad m =>  Trace m -> Text -> TraceTransformer -> IO ()
insertInOracle (ctx, _) name trans =
    modifyMVar_ ctx (\(TraceController mapping) -> return $ TraceController $ insert name trans mapping)

transformTrace :: Text -> Trace IO -> IO (TraceTransformer, Trace IO)
transformTrace name tr@(ctx, _) = do
    traceTransformer <- oracle tr name
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

