
\subsection{Trace}

\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      TraceNamedE
    , stdoutTrace
    , noTrace
    , emptyContext
    -- , aggregateTrace
    -- * context naming
    , appendName
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


import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar,
                     withMVar)

import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import           Data.Bool (bool)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Map (Map, findWithDefault, insert)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Unique (Unique, hashUnique, newUnique)

import           GHC.Generics (Generic)

import           Cardano.BM.Aggregation

import           System.IO.Unsafe (unsafePerformIO)


newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

type TraceNamed m = Trace m (LogNamed LogObject)

type LoggerName = Text

-- Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: [LoggerName]
    , lnItem :: item
    } deriving (Show)

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


-- log item
data LogItem = LogItem
    { liSelection :: LogSelection
    , liSeverity  :: Severity
    , liPayload   :: Text   -- TODO should become ToObject
    } deriving (Show, Generic, ToJSON)

traceNamedObject
    :: TraceNamedE m
    -> LogObject
    -> m ()
traceNamedObject (_, logTrace) obj =
    traceWith (named logTrace) obj

traceNamedItem
    :: TraceNamedE m
    -> LogSelection
    -> Severity
    -> Text
    -> m ()
traceNamedItem (_, logTrace) p s m =
    traceWith (named logTrace) $ LP $ LogMessage $ LogItem { liSelection = p
                                                           , liSeverity  = s
                                                           , liPayload   = m
                                                           }

-- contramap :: (a -> b) -> f b -> f a
-- contramap :: (LogItem -> LogNamed LogItem) -> Trace m (LogNamed LogItem) -> Trace m LogItem

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

traceWith :: Trace m s -> s -> m ()
traceWith = getOp . runTrace

-- getOp :: Op ((m ()) s) -> s -> m ()
-- runTrace :: Op ((m ()) s)

natTrace :: (forall x . m x -> n x) -> Trace m s -> Trace n s
natTrace nat (Trace (Op tr)) = Trace $ Op $ nat . tr

-- add/modify named context
modifyName
    :: ([LoggerName] -> [LoggerName])
    -> TraceNamed m
    -> TraceNamed m
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: Text -> TraceNamedE m -> TraceNamedE m
appendName lname (c,ltr) = (c, modifyName (\e -> [lname] <> e) ltr)

-- return a Trace from a TraceNamed
named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty)

-- serialize output  -- TODO remove it
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()

-- doesn't force the logged messages.
noTrace :: Applicative m => Trace m a
noTrace = Trace $ Op $ const (pure ())

-- 'Trace' to stdout.
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

logMessage, logMessageS, logMessageP :: TraceNamedE m -> Severity -> Text -> m ()
logMessage logTrace  = traceNamedItem logTrace Both
logMessageS logTrace = traceNamedItem logTrace Private
logMessageP logTrace = traceNamedItem logTrace Public

logDebug, logInfo, logNotice, logWarning, logError
    :: TraceNamedE m -> Text -> m ()
logDebug logTrace   = traceNamedItem logTrace Both Debug
logInfo logTrace    = traceNamedItem logTrace Both Info
logNotice logTrace  = traceNamedItem logTrace Both Notice
logWarning logTrace = traceNamedItem logTrace Both Warning
logError logTrace   = traceNamedItem logTrace Both Error
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: TraceNamedE m -> Text -> m ()
logDebugS logTrace   = traceNamedItem logTrace Private Debug
logInfoS logTrace    = traceNamedItem logTrace Private Info
logNoticeS logTrace  = traceNamedItem logTrace Private Notice
logWarningS logTrace = traceNamedItem logTrace Private Warning
logErrorS logTrace   = traceNamedItem logTrace Private Error
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: TraceNamedE m -> Text -> m ()
logDebugP logTrace   = traceNamedItem logTrace Public Debug
logInfoP logTrace    = traceNamedItem logTrace Public Info
logNoticeP logTrace  = traceNamedItem logTrace Public Notice
logWarningP logTrace = traceNamedItem logTrace Public Warning
logErrorP logTrace   = traceNamedItem logTrace Public Error

logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: TraceNamedE m -> Text -> m ()
logDebugUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Debug
logInfoUnsafeP logTrace    = traceNamedItem logTrace PublicUnsafe Info
logNoticeUnsafeP logTrace  = traceNamedItem logTrace PublicUnsafe Notice
logWarningUnsafeP logTrace = traceNamedItem logTrace PublicUnsafe Warning
logErrorUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Error

---------------------

data LogPrims = LogMessage LogItem | LogValue Text Integer deriving (Generic, Show, ToJSON)
data LogObject = LP LogPrims
               | ObserveOpen CounterState
               | ObserveClose CounterState [LogPrims]
               deriving (Generic, Show, ToJSON)

stmWithLog :: STM.STM t -> STM.STM (t, [LogObject])
stmWithLog action = do
    t <- action
    return (t, [LP (LogMessage (LogItem Both Info "enter")),LP (LogMessage (LogItem Both Info "leave"))])

-- stmWithLog :: STM.STM t -> STM.STM (t, [LogPrims])
-- stmWithLog action = do
--      t <- action
--      return (t, [LogMessage “enter”, LogMessage “leave”])

type Counter = POSIXTime

data CounterState =
    CounterState Unique [Counter]
  | EmptyCounterState Unique
        deriving (Generic, Show, ToJSON)

stateIdentifier :: CounterState -> Unique
stateIdentifier (CounterState      x _) = x
stateIdentifier (EmptyCounterState x)   = x

instance Generic Unique where

instance ToJSON Unique where
    toJSON = toJSON . hashUnique
    toEncoding = toEncoding . hashUnique

instance Show Unique where
    show = show . hashUnique

example :: {-TraceNamed m ->-} IO ()
example = do
    let logTrace0 = stdoutTrace
    ctx <- newMVar $ TraceController $ mempty
    let logTrace = appendName "my_example" (ctx, logTrace0)
    insertInOracle logTrace "expect_answer" Neutral -- DropOpening
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

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

setupTrace :: MonadIO m => TraceConfiguration -> m (TraceNamedE m)
setupTrace (TraceConfiguration outputKind name) = do
    ctx <- liftIO $ emptyContext -- newMVar $ TraceController $ mempty
    let logTrace0 = bool noTrace (natTrace liftIO stdoutTrace) (outputKind == StdOut)
    let logTrace = (ctx, logTrace0)
    liftIO $ insertInOracle logTrace name Neutral
    return logTrace

withTrace :: MonadIO m =>  TraceConfiguration -> (TraceNamedE m -> m ()) -> m ()
withTrace cfg action = do
    logTrace <- setupTrace cfg
    action logTrace

bracketObserveIO :: TraceNamedE IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
    (traceTransformer, logTrace) <- transformTrace name logTrace0
    countersid <- observeOpen traceTransformer logTrace
    -- run action, return result and log items
    (t, as) <- STM.atomically $ stmWithLog action
    observeClose traceTransformer logTrace countersid as
    pure t

observeOpen :: TraceTransformer -> TraceNamedE IO -> IO CounterState
observeOpen traceTransformer logTrace = do
    identifier <- newUnique
    logInfo logTrace $ "Opening: " <> pack (show $ hashUnique identifier)

    -- take measurement only if needed
    state <- case traceTransformer of
        NoTrace      -> return $ EmptyCounterState identifier
        UntimedTrace -> return $ EmptyCounterState identifier
        Neutral -> do
            counters <- readCounters
            return $ CounterState identifier counters
    -- send opening message to Trace
    traceNamedObject logTrace $ ObserveOpen state
    return state

observeClose :: TraceTransformer -> TraceNamedE IO -> CounterState -> [LogObject] -> IO ()
observeClose traceTransformer logTrace counterState logObjects = do
    let identifier = stateIdentifier counterState
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)
    let msgs = filterPrims logObjects

    -- take measurement only if needed
    state <- case traceTransformer of
        NoTrace      -> return $ EmptyCounterState identifier
        UntimedTrace -> return $ EmptyCounterState identifier
        Neutral -> do
            counters <- readCounters
            -- log the diff between the counters
            case counterState of
                CounterState _ counters0 ->
                    logInfo logTrace $ "diff counters: "
                        <> pack (show ( zipWith
                                            (\a b -> nominalDiffTimeToMicroseconds (b - a))
                                            counters0
                                            counters))
                _ -> pure ()
            return $ CounterState identifier counters
    -- send closing message to Trace
    traceNamedObject logTrace $ ObserveClose state msgs
    -- trace the messages gathered from inside the action
    -- TODO what about ObserveOpen or ObserveClose inside STM action??
    forM_ msgs $ traceNamedObject logTrace . LP
  where
    filterPrims :: [LogObject] -> [LogPrims]
    filterPrims = mapMaybe (\case
                                (LP a) -> Just a
                                _      -> Nothing)

dropPrims :: [LogObject] -> [LogObject]
dropPrims = filter (\case {LP _ -> False; _ -> True})

readCounters :: IO [Counter]
readCounters = do
    time <- getPOSIXTime
    return [time]

nominalDiffTimeToMicroseconds :: POSIXTime -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . round . (* 1000000)

data TraceTransformer = Neutral
                      | UntimedTrace
                      | NoTrace
                      | DropOpening
                      | DropClosing
                      | Aggregate
                      | ListTrace (STM.TVar [LogObject])

traceInTVar :: STM.TVar [LogObject] -> Trace STM.STM LogObject
traceInTVar tvar = Trace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = Trace $ Op $ \lognamed -> STM.atomically $ STM.modifyTVar tvar ((:) (lnItem lognamed))

oracle :: TraceNamedE m -> Text -> IO TraceTransformer
oracle (ctx, _) name =
	getTraceTransformer ctx name

type TraceNamedE m = (TraceContext, Trace m (LogNamed LogObject))

type TraceContext = MVar TraceController
data TraceController = TraceController {
    traceTransformers :: Map Text TraceTransformer
    -- ...
    }

data TraceConfiguration = TraceConfiguration
  { tcOutputKind :: OutputKind
  , tcName       :: Text
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

insertInOracle :: Monad m =>  TraceNamedE m -> Text -> TraceTransformer -> IO ()
insertInOracle (ctx, _) name trans =
    modifyMVar_ ctx (\(TraceController map) -> return $ TraceController $ insert name trans map)

transformTrace :: Text -> TraceNamedE IO -> IO (TraceTransformer, TraceNamedE IO)
transformTrace name tr@(ctx, logTrace0) = do
    traceTransformer <- oracle tr name
    return $ case traceTransformer of
        Neutral     -> (traceTransformer, appendName name tr)
        NoTrace     -> (traceTransformer, (ctx, Trace $ Op $ \_ -> pure ()))
        DropOpening -> (traceTransformer, (ctx, Trace $ Op $ \lognamed ->
            case lnItem lognamed of
                ObserveOpen _ -> return ()
                obj           -> traceNamedObject tr obj))
        ListTrace tvar -> (traceTransformer, (ctx, traceInTVarIO tvar))
\end{code}
