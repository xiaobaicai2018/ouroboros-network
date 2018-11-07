{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Unique (Unique, hashUnique, newUnique)

import           GHC.Generics (Generic)

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
    deriving (Show, Generic, ToJSON)

-- | severity of log message
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show, Eq, Ord, Generic, ToJSON)


-- | log item
data LogItem = LogItem
    { liSelection :: LogSelection
    , liSeverity  :: Severity
    , liPayload   :: Text   -- TODO should become ToObject
    } deriving (Show, Generic, ToJSON)

traceNamedObject
    :: TraceNamed m
    -> LogObject
    -> m ()
traceNamedObject logTrace o =
    traceWith (named logTrace) o

traceNamedItem
    :: TraceNamed m
    -> LogSelection
    -> Severity
    -> Text
    -> m ()
traceNamedItem logTrace p s m =
    traceWith (named logTrace) $ LP $ LogMessage $ LogItem { liSelection = p
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
        LP (LogMessage logItem) -> do
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed) <> " :: " <> (liPayload logItem)
        -- obj -> do
        --     withMVar locallock $ \_ ->
        --         TIO.putStrLn $ contextname (lnName lognamed)
        --             <> " :: " <> (pack . show . toJSON) obj
        -- TODO better ToJSON instance for LogObject
        (ObserveOpen (CounterState identifier cs)) -> do
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed)
                    <> " :: ObserveOpen " <> pack (show (hashUnique identifier))
                    <> " :: " <> (pack (show cs))
        (ObserveClose (CounterState identifier cs) _) -> do
            withMVar locallock $ \_ ->
                TIO.putStrLn $ contextname (lnName lognamed)
                    <> " :: ObserveClose " <> pack (show (hashUnique identifier))
                    <> " :: " <> (pack (show cs))
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

data LogPrims = LogMessage LogItem | LogValue Text Integer deriving (Generic, ToJSON)
data LogObject = LP LogPrims
  		       | ObserveOpen CounterState
  		       | ObserveClose CounterState [LogPrims]
                    deriving (Generic, ToJSON)

stmWithLog :: STM.STM t -> STM.STM (t, [LogObject])
stmWithLog action = do
    t <- action
    return (t, [LP (LogMessage (LogItem Both Info "enter")),LP (LogMessage (LogItem Both Info "leave"))])

-- stmWithLog :: STM.STM t -> STM.STM (t, [LogPrims])
-- stmWithLog action = do
--      t <- action
--      return (t, [LogMessage “enter”, LogMessage “leave”])


type Counter = POSIXTime

data CounterState = CounterState Unique [Counter] deriving (Generic, ToJSON)

instance Generic Unique where

instance ToJSON Unique where
    toJSON = toJSON . hashUnique
    toEncoding = toEncoding . hashUnique

example :: {-TraceNamed m ->-} IO ()
example = do
    let logTrace = appendName "my_example" stdoutTrace
    result <- bracketObserveIO logTrace "expect_answer" setVar_
    logInfo logTrace $ pack $ show result

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

bracketObserveIO :: TraceNamed IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
  logTrace <- transformTrace name logTrace0
  countersid <- observeOpen logTrace
  -- run action, return result and log items
  (t, as) <- STM.atomically $ stmWithLog action
  observeClose logTrace countersid as
  pure t

observeOpen :: TraceNamed IO -> IO CounterState
observeOpen logTrace = do
    identifier <- newUnique
    -- take measurement
    counters   <- readCounters
    logInfo logTrace $ "Opening: " <> pack (show $ hashUnique identifier)
    -- send opening message with measurement to Trace
    let state = CounterState identifier counters
    traceNamedObject logTrace $ ObserveOpen state
    return state

observeClose :: TraceNamed IO -> CounterState -> [LogObject] -> IO ()
observeClose logTrace (CounterState identifier counters0) logObjects = do
    -- take measurement
    counters <- readCounters
    logInfo logTrace $ "diff counters: "
        <> pack (show $ zipWith (\a b -> nominalDiffTimeToMicroseconds (b - a)) counters0 counters)
    -- here: send closing message to Trace (with diff of counters)
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)
    let msgs = filterPrims logObjects
    -- pass measurement to trace
    traceNamedObject logTrace $ ObserveClose (CounterState identifier counters) msgs
    -- trace the messages gathered from inside the action
    -- TODO what about ObserveOpen or ObserveClose inside STM action??
    forM_ msgs $ traceNamedObject logTrace . LP
  where
    filterPrims :: [LogObject] -> [LogPrims]
    filterPrims = mapMaybe (\case
                                (LP a) -> Just a
                                _      -> Nothing)

readCounters :: IO [Counter]
readCounters = do
    time <- getPOSIXTime
    return [time]

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

data TraceTransformer = Neutral
                      | NoTrace
                      | DropOpening
                      | DropClosing
                      | Aggregate
                      | ListTrace (STM.TVar [LogObject])

traceInTVar :: STM.TVar [LogObject] -> Trace STM.STM LogObject
traceInTVar tvar = Trace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = Trace $ Op $ \lognamed -> STM.atomically $ STM.modifyTVar tvar ((:) (lnItem lognamed))

oracle :: TraceNamed m -> Text -> IO TraceTransformer
oracle _ _ = return Neutral -- DropOpening
-- TODO
-- oracle (lh, _) name = do
-- 	confighandler <- getConfigHandler lh
-- 	return $ getTransformer confighandler name

-- TraceNamed m = (LogHandler, Trace m (LogObject))

transformTrace :: Text -> TraceNamed IO -> IO (TraceNamed IO)
transformTrace name {-(lh, -}logTrace0{-)-} = do
    traceTransformer <- oracle logTrace0 name
    return $ case traceTransformer of
        Neutral -> appendName name logTrace0
        NoTrace -> {-(lh, -}Trace $ Op $ \_ -> pure (){-)-}
        DropOpening -> {-(lh, -}Trace $ Op $ \lognamed ->
            case lnItem lognamed of
                ObserveOpen _ -> return ()
                obj           -> traceNamedObject logTrace0 obj {-)-}
        ListTrace tvar -> {-(lh, -}traceInTVarIO tvar{-)-}
