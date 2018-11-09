
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

    , TraceConfiguration (..)
    , TraceTransformer (..)
    , OutputKind (..)
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

import           System.IO.Unsafe (unsafePerformIO)
\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}
A |Trace| consists of a \nameref{code:TraceContext} and a \nameref{code:TraceNamed} in |m|.
\begin{code}

type Trace m = (TraceContext, TraceNamed m)
\end{code}

\subsubsection{TraceNamed}\label{code:TraceNamed}
A |TraceNamed| is a trace of types |LogNamed| with payload |LogObject|.
\begin{code}

type TraceNamed m = BaseTrace m (LogNamed LogObject)
\end{code}

A |LogNamed| contains a list of context names and some log item.
\begin{code}
type ContextName = Text

-- Attach a 'ContextName' to something.
data LogNamed item = LogNamed
    { lnName :: [ContextName]
    , lnItem :: item
    } deriving (Show)

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

\subsubsection{LogItem}\label{code:LogItem}
\begin{code}

-- log item
data LogItem = LogItem
    { liSelection :: LogSelection
    , liSeverity  :: Severity
    , liPayload   :: Text   -- TODO should become ToObject
    } deriving (Show, Generic, ToJSON)

-- output selection
data LogSelection =
      Public       -- only to public logs.
    | PublicUnsafe -- only to public logs, not console.
    | Private      -- only to private logs.
    | Both         -- to public and private logs.
    deriving (Show, Generic, ToJSON)

-- severity of log message
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show, Eq, Ord, Generic, ToJSON)

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

\subsubsection{Concrete Trace on stdout}
\begin{code}
stdoutTrace :: TraceNamed IO
stdoutTrace = BaseTrace $ Op $ \lognamed ->
    case lnItem lognamed of
        LP (LogMessage logItem) ->
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed) <> " :: " <> (liPayload logItem)
        obj ->
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed)
                    <> " :: " <> toStrict (encodeToLazyText obj)
  where
    contextname :: [ContextName] -> Text
    contextname (y : ys) = foldl (\e a -> e <> "." <> a) y ys
    contextname []       = "(null name)"

\end{code}

\subsubsection{Enter messages into a trace}
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

\label{code:LogPrims}\label{LogObject}
\begin{code}

data LogPrims = LogMessage LogItem
              | LogValue Text Integer
                deriving (Generic, Show, ToJSON)

data LogObject = LP LogPrims
               | ObserveOpen CounterState
               | ObserveClose CounterState [LogPrims]
                 deriving (Generic, Show, ToJSON)
\end{code}

\begin{code}

stmWithLog :: STM.STM t -> STM.STM (t, [LogObject])
stmWithLog action = do
    t <- action
    return (t, [LP (LogMessage (LogItem Both Info "enter")),LP (LogMessage (LogItem Both Info "leave"))])
\end{code}

\begin{code}

type Bytes = Integer

data Counter = MonotonicClockTime Microsecond
             | MemoryResidency Bytes
               deriving (Show, Generic, ToJSON)

instance ToJSON Microsecond where
    toJSON = toJSON . toMicroseconds
    toEncoding = toEncoding . toMicroseconds

data CounterState = CounterState {
    csIdentifier :: Unique
  , csCounters :: [Counter]
  }
    deriving (Generic, Show, ToJSON)

instance Generic Unique where

instance ToJSON Unique where
    toJSON = toJSON . hashUnique
    toEncoding = toEncoding . hashUnique

instance Show Unique where
    show = show . hashUnique

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
readCounters (ObservableTrace set) = foldrM (\(sel, fun) a ->
    if sel `member` set then (fun >>= \xs -> return $ a ++ xs) else return a) [] selectors
  where
    selectors = [(MonotonicClock, getMonoClock), (MemoryStats, readMemStats){-, (CPUTimeStats, readCPUTimeStats)-}]
    getMonoClock :: IO [Counter]
    getMonoClock = do
        t <- getMonotonicTimeNSec
        let meas = MonotonicClockTime $ nominalDiffTimeToMicroseconds t
        return $ [meas]
    readMemStats :: IO [Counter]
    readMemStats = return [MemoryResidency (-1), MemoryResidency (-2)]

\end{code}

\begin{code}

traceNamedObject
    :: Trace m
    -> LogObject
    -> m ()
traceNamedObject (_, logTrace) = traceWith (named logTrace)

\end{code}

\begin{code}
bracketObserveIO :: Trace IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
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

nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)

\end{code}

\begin{code}
data TraceTransformer = Neutral
                      | UntimedTrace
                      | NoTrace
                      | DropOpening
                      | ListTrace (STM.TVar [LogObject])
                      | ObservableTrace (Set ObservableInstance)

data ObservableInstance = MonotonicClock
                        | MemoryStats
                        | CPUTimeStats
                          deriving (Eq, Ord)
\end{code}

\begin{code}

traceInTVar :: STM.TVar [LogObject] -> BaseTrace STM.STM LogObject
traceInTVar tvar = BaseTrace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = BaseTrace $ Op $ \lognamed -> STM.atomically $ STM.modifyTVar tvar ((:) (lnItem lognamed))

oracle :: Trace m -> Text -> IO TraceTransformer
oracle (ctx, _) = getTraceTransformer ctx

\end{code}

\begin{code}

type TraceContext = MVar TraceController
data TraceController = TraceController {
    traceTransformers :: Map Text TraceTransformer
    }

data TraceConfiguration = TraceConfiguration
  { tcOutputKind       :: OutputKind
  , tcName             :: Text
  , tcTraceTransformer :: TraceTransformer
  }

data OutputKind = StdOut | Null deriving Eq

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
    modifyMVar_ ctx (\(TraceController map) -> return $ TraceController $ insert name trans map)

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

