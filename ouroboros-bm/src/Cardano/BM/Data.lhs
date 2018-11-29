
\subsection{Data}

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Data
  (
    Trace
  , TraceNamed
  , TraceConfiguration (..)
  , TraceContext (..)
  , TraceController (..)
  , SubTrace (..)
  , TraceTransformerMap
  , OutputKind (..)
  , LogPrims (..)
  , LogObject (..)
  , ObservableInstance (..)
  , NamedLogItem
  , LogNamed (..)
  , LogItem (..)
  , LogSelection (..)
  , LoggerName
  , Severity (..)
  , Counter (..)
  , CounterState (..)
  , Backend (..)
  , ScribeKind (..)
  , HasPass (..)
  )
  where

import qualified Control.Concurrent.STM.TVar as STM

import           Control.Concurrent.MVar (MVar)
--import           Control.Monad.IO.Class (MonadIO)

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
%endif

\subsubsection{Accepts a \nameref{code:NamedLogItem}}\label{code:HasPass}
\begin{code}
class HasPass t where
    pass :: t -> NamedLogItem -> IO ()

\end{code}

\subsubsection{Type of a logged item}\label{code:NamedLogItem}
\begin{code}
type NamedLogItem = LogNamed LogObject

\end{code}

\subsubsection{Trace}\label{code:Trace}
A |Trace| consists of a \nameref{code:TraceContext} and a \nameref{code:TraceNamed} in |m|.
\begin{code}

type Trace m = (TraceContext, TraceNamed m)
\end{code}

\subsubsection{TraceNamed}\label{code:TraceNamed}
A |TraceNamed| is a specialized \nameref{code:BaseTrace} of type \nameref{code:LogNamed} with payload \nameref{code:LogObject}.
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

-- Attach a 'ContextName' and Katip related info to something.
data LogNamedPlus item = LogNamedPlus
    { lnpName :: LoggerName

    , lnpItem :: item
    } deriving (Show)


\end{code}

\subsubsection{SubTrace}\label{code:SubTrace}
\begin{code}
data SubTrace = Neutral
              | UntimedTrace
              | NoTrace
              | DropOpening
              | ObservableTrace (Set ObservableInstance)
                deriving (Show)
\end{code}

\subsubsection{ObservableInstance}\label{code:ObservableInstance}
\begin{code}
data ObservableInstance = MonotonicClock
                        | MemoryStats
                        | ProcessStats
                        | IOStats
                          deriving (Eq, Ord, Show)
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

instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
                    "Debug"    -> pure Debug
                    "Info"     -> pure Info
                    "Notice"   -> pure Notice
                    "Warning"  -> pure Warning
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
               deriving (Eq, Show, Generic, ToJSON)

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
We keep the context's name and a reference to the |TraceController|
in the |TraceContext|.
\begin{code}

type LoggerName = Text

data TraceContext = TraceContext {
      loggerName :: LoggerName
    , controller :: MVar TraceController
    }

\end{code}

\subsubsection{TraceController}\label{code:TraceController}
\todo[inline]{TODO replace the |TraceController| with access to \nameref{Configuration}}

\begin{code}
type TraceTransformerMap = Map LoggerName SubTrace
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
  , tcTraceTransformer :: SubTrace
  , tcSeverity         :: Severity
  }

\end{code}

\subsubsection{TraceConfiguration}\label{code:TraceConfiguration}
\begin{code}

data OutputKind = StdOut
                | TVarList (STM.TVar [LogObject])
                | TVarListNamed (STM.TVar [LogNamed LogObject])
                | Null
                deriving Eq

\end{code}

\subsubsection{Backend}\label{code:Backend}
A backend is referenced through the function |pass'| which accepts
a \nameref{code:NamedLogItem}.

\begin{code}
data Backend = MkBackend { pass' :: NamedLogItem -> IO () }

\end{code}

\subsubsection{ScribeKind}\label{code:ScribeKind}
This identifies katip's scribes by type.

\begin{code}
data ScribeKind = FileTextSK
                | FileJsonSK
                | StdoutSK
                | StderrSK
                | DevNullSK
                deriving (Eq, Show)

\end{code}

\subsubsection{BackendKind}\label{code:BackendKind}
This identifies the backends that can be attached to the \nameref{code:Switchboard}.

\begin{code}
data BackendKind = AggregationBK
                 | EKGViewBK
                 | KatipBK
                 | DevNullBK
                 deriving (Eq, Show)

\end{code}

