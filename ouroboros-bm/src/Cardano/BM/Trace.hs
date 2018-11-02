{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      TraceNamed
    , stdoutTrace
    , aggregateTrace
    -- * context naming
    , appendName
    , modifyName
    -- * utils
    , natTrace
    -- * log functions
    , logObservable
    , logMessage, logMessageS, logMessageP
    , logDebug,   logDebugS,   logDebugP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningUnsafeP
    ) where


import           Control.Concurrent.MVar (MVar, newMVar, withMVar)

import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.HashMap.Lazy (HashMap, alter)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO

import           System.IO.Unsafe (unsafePerformIO)


-- | base Trace
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

type TraceNamed m = Trace m (LogNamed LogItem)

type LoggerName = Text

data Aggregation = Aggregation {
    fmin :: Integer,
    fmax :: Integer,
    fmean :: Integer,
    fcount :: Integer,
    fsumA :: Integer,  -- TODO 
    fsumB :: Integer   -- TODO
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
    , lnAggregation :: HashMap [LoggerName] Aggregation
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
    { liSelection  :: LogSelection
    , liSeverity   :: Severity
    , liPayload    :: Text   -- TODO should become ToObject
    } deriving (Show)

traceNamedItem
    :: TraceNamed m
    -> LogSelection
    -> Severity
    -> Text
    -> m ()
traceNamedItem logTrace p s m =
    traceWith (named logTrace) LogItem { liSelection = p
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
    f (LogNamed name aggr item) = LogNamed (k name) aggr item

appendName :: Text -> TraceNamed m -> TraceNamed m
appendName lname = modifyName (\e -> [lname] <> e)

-- | return a Trace from a TraceNamed
named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty mempty)

-- | serialize output  -- TODO remove it
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()

-- | 'Trace' to stdout.
stdoutTrace :: TraceNamed IO
stdoutTrace = Trace $ Op $ \lognamed -> do
    withMVar locallock $ \_ ->
        TIO.putStrLn $ contextname (lnName lognamed) <> " :: " <> (liPayload $ lnItem lognamed)
  where
    contextname :: [LoggerName] -> Text
    contextname = foldr (\e a -> a <> "." <> e) ""

-- | Trace which aggregates online
aggregateTrace :: TraceNamed IO
aggregateTrace = Trace $ Op $ \lognamed -> do
    withMVar locallock $ \_ -> do
        TIO.putStrLn $ contextname (lnName lognamed) <> " :: " <> (liPayload $ lnItem lognamed)
        TIO.putStrLn $ "   " <> pack(show $ lnAggregation lognamed)
  where
    contextname :: [LoggerName] -> Text
    contextname = foldr (\e a -> a <> "." <> e) ""
    

-- | logging functions
logObservable :: TraceNamed m -> LoggerName -> Integer -> TraceNamed m
logObservable logTrace context value = 
    contramap f logTrace
  where
    f (LogNamed name aggr item) = LogNamed name (update name aggr) item
    update name aggr = alter (updateAggregation value) (name <> [context]) aggr 

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

