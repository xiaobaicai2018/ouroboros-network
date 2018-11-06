{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      TraceNamed
    , stdoutTrace
    -- , aggregateTrace
    -- * context naming
    , appendName
    , modifyName
    -- * utils
    , natTrace
    -- * log functions
    , logMessage, logMessageS, logMessageP
    , logDebug,   logDebugS,   logDebugP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningUnsafeP
    , example
    ) where


import           Control.Concurrent.MVar (MVar, newMVar, withMVar)

import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forM_)
import qualified Control.Monad.STM as STM

import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Unique (Unique, hashUnique, newUnique)

import           System.IO.Unsafe (unsafePerformIO)


-- | base Trace
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

type TraceNamed m = Trace m (LogNamed LogObject)

type LoggerName = Text

data Aggregation = Aggregation {
    fmin   :: Integer,
    fmax   :: Integer,
    fmean  :: Integer,
    fcount :: Integer,
    fsumA  :: Integer,  -- TODO
    fsumB  :: Integer   -- TODO
    } deriving (Show)

updateAggregation :: Integer -> Maybe Aggregation -> Maybe Aggregation
updateAggregation v Nothing =
    Just $
    Aggregation { fmin=v
                , fmax=v
                , fmean=v
                , fcount=1
                , fsumA=v
                , fsumB=v * v }
updateAggregation v (Just (Aggregation _min _max _mean _count _sumA _sumB)) =
    Just $
    Aggregation { fmin=(min _min v)
                , fmax=(max _max v)
                , fmean=((_sumA + v) `div` (_count + 1))
                , fcount=(_count + 1)
                , fsumA=(_sumA + v)
                , fsumB=(_sumB + v * v) }

-- | Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: [LoggerName]
    , lnItem :: item
    } deriving (Show)

-- | output selection
data LogSelection =
      Public       -- only to public logs.
    | PublicUnsafe -- only to public logs, not console.
    | Private      -- only to private logs.
    | Both         -- to public and private logs.
    deriving (Show)

-- | severity of log message
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show, Eq, Ord)


-- | log item
data LogItem = LogItem
    { liSelection :: LogSelection
    , liSeverity  :: Severity
    , liPayload   :: Text   -- TODO should become ToObject
    } deriving (Show)

traceNamedItem
    :: TraceNamed m
    -> LogSelection
    -> Severity
    -> Text
    -> m ()
traceNamedItem logTrace p s m =
    traceWith (named logTrace) $ LogMessage $
                               LogItem { liSelection = p
                                       , liSeverity  = s
                                       , liPayload   = m
                                       }


natTrace :: (forall x . m x -> n x) -> Trace m s -> Trace n s
natTrace nat (Trace (Op tr)) = Trace $ Op $ nat . tr

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

traceWith :: Trace m s -> s -> m ()
traceWith = getOp . runTrace

-- | add/modify named context
modifyName
    :: ([LoggerName] -> [LoggerName])
    -> TraceNamed m
    -> TraceNamed m
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: Text -> TraceNamed m -> TraceNamed m
appendName lname = modifyName (\e -> [lname] <> e)

-- | return a Trace from a TraceNamed
named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty)

-- | serialize output  -- TODO remove it
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()

-- | 'Trace' to stdout.
stdoutTrace :: TraceNamed IO
stdoutTrace = Trace $ Op $ \lognamed ->
    case lnItem lognamed of
        (LogMessage logItem) -> do
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed) <> " :: " <> (liPayload logItem)
        _ -> pure ()
  where
    contextname :: [LoggerName] -> Text
    contextname (y : ys) = foldl (\e a -> e <> "." <> a) y ys
    contextname []       = "(null name)"

logMessage, logMessageS, logMessageP :: TraceNamed m -> Severity -> Text -> m ()
logMessage logTrace  = traceNamedItem logTrace Both
logMessageS logTrace = traceNamedItem logTrace Private
logMessageP logTrace = traceNamedItem logTrace Public

logDebug, logInfo, logNotice, logWarning, logError
    :: TraceNamed m -> Text -> m ()
logDebug logTrace   = traceNamedItem logTrace Both Debug
logInfo logTrace    = traceNamedItem logTrace Both Info
logNotice logTrace  = traceNamedItem logTrace Both Notice
logWarning logTrace = traceNamedItem logTrace Both Warning
logError logTrace   = traceNamedItem logTrace Both Error
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: TraceNamed m -> Text -> m ()
logDebugS logTrace   = traceNamedItem logTrace Private Debug
logInfoS logTrace    = traceNamedItem logTrace Private Info
logNoticeS logTrace  = traceNamedItem logTrace Private Notice
logWarningS logTrace = traceNamedItem logTrace Private Warning
logErrorS logTrace   = traceNamedItem logTrace Private Error
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: TraceNamed m -> Text -> m ()
logDebugP logTrace   = traceNamedItem logTrace Public Debug
logInfoP logTrace    = traceNamedItem logTrace Public Info
logNoticeP logTrace  = traceNamedItem logTrace Public Notice
logWarningP logTrace = traceNamedItem logTrace Public Warning
logErrorP logTrace   = traceNamedItem logTrace Public Error

logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: TraceNamed m -> Text -> m ()
logDebugUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Debug
logInfoUnsafeP logTrace    = traceNamedItem logTrace PublicUnsafe Info
logNoticeUnsafeP logTrace  = traceNamedItem logTrace PublicUnsafe Notice
logWarningUnsafeP logTrace = traceNamedItem logTrace PublicUnsafe Warning
logErrorUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Error

---------------------

data LogObject = LogMessage LogItem | LogValue Integer deriving Show

stmNoTrace :: STM.STM t -> STM.STM (t, [LogObject])
stmNoTrace action = do
    t <- action
    return (t, [LogMessage (LogItem Both Info "enter"), LogMessage (LogItem Both Info "leave")])

type Counter = POSIXTime -- Integer
data CounterState = CounterState Unique [Counter]

example :: IO ()
example = do
    let logTrace = appendName "my_example" stdoutTrace
    result <- bracketObserveIO_ logTrace "expect_answer" setVar_
    logInfo logTrace $ pack $ show result

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

bracketObserveIO_ :: TraceNamed IO -> Text -> STM.STM t -> IO t
bracketObserveIO_ logTrace0 name action = do
    (logTrace, countersid) <- observeOpen logTrace0 name
    (t, as) <- STM.atomically $ stmNoTrace action   -- run action, return result and log items
    -- propagate outcome to TraceNamed and print
    observeClose logTrace countersid as
    -- final stuff
    --
    -- combine local
    pure t

observeOpen :: TraceNamed IO -> Text -> IO (TraceNamed IO, CounterState)
observeOpen logTrace0 name = do
    let logTrace = appendName name logTrace0
    counters <- readCounters
    identifier <- newUnique
    logInfo logTrace $ "Opening: " <> pack (show $ hashUnique identifier)
    -- here: send opening message to Trace
    return (logTrace, CounterState identifier counters)

readCounters :: IO [Counter]
readCounters = do
    time <- getPOSIXTime
    return [time]

observeClose :: TraceNamed IO -> CounterState -> [LogObject] -> IO ()
observeClose logTrace (CounterState identifier counters0) as = do
    counters <- readCounters
    -- here: send closing message to Trace (with diff of counters)
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)
    logInfo logTrace $ "diff counters: " <> (pack $ (show $ zipWith (\a b -> nominalDiffTimeToMicroseconds (b - a)) counters0 counters))
    -- here: send `as` to Trace (list of messages, list of values)
    -- let (msg, vals) = partition (\case {(LogMessage _) -> True; _ -> False}) as
    -- forM_ msgs (\LogMessage t -> traceNamedItem logTrace t)
    -- traceNamedItem logTrace $ aggregate vals
    forM_ as $ (\case
        (LogMessage t) -> traceNamedItem logTrace (liSelection t) (liSeverity t) (liPayload t)
        a              -> logInfo logTrace (pack $ show a)
        )

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)
