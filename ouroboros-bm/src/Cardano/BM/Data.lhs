
\subsection{Data}

\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data
  (
    Trace
  , TraceNamed
  , TraceConfiguration (..)
  , TraceContext (..)
  , TraceController (..)
  , TraceTransformer (..)
  , TraceTransformerMap
  , OutputKind (..)
  , LogPrims (..)
  , LogObject (..)
  , ObservableInstance (..)
  , LogNamed (..)
  , LogItem (..)
  , LogSelection (..)
  , LoggerName
  , Severity (..)
  , Counter (..)
  , CounterState (..)
  , diffTimeObserved
  )
  where

import qualified Control.Concurrent.STM.TVar as STM

import           Control.Concurrent.MVar (MVar)

import           Data.Aeson (FromJSON (..), ToJSON, toEncoding, toJSON)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.Unique (Unique, hashUnique)
import           Data.Yaml (withText)

import           GHC.Generics (Generic)

import           Cardano.BM.BaseTrace

\end{code}

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

\subsubsection{LogObject}\label{code:LogObject}
\label{code:LogPrims}\label{code:LogObject}
\begin{code}

data LogPrims = LogMessage LogItem
              | LogValue Text Integer
                deriving (Generic, Show, ToJSON)

data LogObject = LP LogPrims
               | ObserveOpen CounterState
               | ObserveClose CounterState
                 deriving (Generic, Show, ToJSON)
\end{code}

\subsubsection{TraceTransformer}\label{code:TraceTransformer}


\begin{code}
data TraceTransformer = Neutral
                      | UntimedTrace
                      | NoTrace
                      | DropOpening
                      | ObservableTrace (Set ObservableInstance)
                        deriving (Show)

data ObservableInstance = MonotonicClock
                        | MemoryStats
                        | ProcessStats
                        | IOStats
                          deriving (Eq, Ord, Show)
\end{code}

\todo[inline]{TODO |lnName :: Text|\newline storing a concatenation of names
 might be cheaper than rebuilding it for every log message}
\subsubsection{LogNamed}\label{code:LogNamed}
A |LogNamed| contains of a list of context names and some log item.
\begin{code}

-- Attach a 'ContextName' to something.
data LogNamed item = LogNamed
    { lnName :: LoggerName
    , lnItem :: item
    } deriving (Show)

deriving instance Generic item => Generic (LogNamed item)
deriving instance (ToJSON item, Generic item) => ToJSON (LogNamed item)

\end{code}

\subsubsection{LogItem}\label{code:LogItem}
\todo[inline]{TODO |liPayload :: ToObject|}

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

-- | Handwritten 'FromJSON' instance because the log config files
--   contain a '+' after their severity that has to be dropped to
--   be parsed into our 'Severity' datatype.
instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
                    "Debug+"   -> pure Debug
                    "Debug"    -> pure Debug
                    "Info+"    -> pure Info
                    "Info"     -> pure Info
                    "Notice+"  -> pure Notice
                    "Notice"   -> pure Notice
                    "Warning+" -> pure Warning
                    "Warning"  -> pure Warning
                    "Error+"   -> pure Error
                    "Error"    -> pure Error
                    _          -> pure Info   -- catch all

\end{code}

\subsubsection{Observable}\label{code:CounterState}
\begin{code}


data Counter = MonotonicClockTime Text Microsecond
             | MemoryCounter Text Integer
             | StatInfo Text Integer
             | IOCounter Text Integer
             | CpuCounter Text Integer
               deriving (Show, Generic, ToJSON)

instance ToJSON Microsecond where
    toJSON = toJSON . toMicroseconds
    toEncoding = toEncoding . toMicroseconds

data CounterState = CounterState {
      csIdentifier :: Unique
    , csCounters :: [Counter]
    }
    deriving (Generic, ToJSON)

instance ToJSON Unique where
    toJSON = toJSON . hashUnique
    toEncoding = toEncoding . hashUnique

instance Show CounterState where
    show cs = (show . hashUnique) (csIdentifier cs)
           <> " => " <> (show $ csCounters cs)

\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}

type LoggerName = Text

data TraceContext = TraceContext {
      loggerName :: LoggerName
    , controller :: MVar TraceController
    }

type TraceTransformerMap = Map LoggerName TraceTransformer
type SeverityMap         = Map LoggerName Severity

data TraceController = TraceController {
      traceTransformers :: TraceTransformerMap
    , severityMap :: SeverityMap
    , minSeverity :: Severity
    }

\end{code}

\subsubsection{TraceConfiguration}\label{code:TraceConfiguration}
\begin{code}

data TraceConfiguration = TraceConfiguration
  { tcOutputKind       :: OutputKind
  , tcName             :: LoggerName
  , tcTraceTransformer :: TraceTransformer
  , tcSeverity         :: Severity
  }

data OutputKind = StdOut
                | TVarList (STM.TVar [LogObject])
                | TVarListNamed (STM.TVar [LogNamed LogObject])
                | Null
                deriving Eq

diffTimeObserved :: CounterState -> CounterState -> Microsecond
diffTimeObserved (CounterState _ startCounters) (CounterState _ endCounters) =
    let
        startTime = getMonotonicTime startCounters
        endTime   = getMonotonicTime endCounters
    in
        endTime - startTime
  where
    getMonotonicTime counters = case (filter isMonotonicClockCounter counters) of
        [(MonotonicClockTime _ micros)] -> micros
        _                               -> error "Exactly one time measurements was expected!"

isMonotonicClockCounter :: Counter -> Bool
isMonotonicClockCounter (MonotonicClockTime _ _) = True
isMonotonicClockCounter _                        = False

\end{code}
