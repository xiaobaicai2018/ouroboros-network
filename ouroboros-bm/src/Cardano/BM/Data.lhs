
\subsection{Data}

\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Data
  where

import qualified Control.Concurrent.STM.TVar as STM

import           Control.Concurrent.MVar (MVar)

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.Unique (Unique, hashUnique)

import           GHC.Generics (Generic)

\end{code}

\subsubsection{LogObject}\label{code:LogObject}
\label{code:LogPrims}\label{code:LogObject}
\begin{code}

data LogPrims = LogMessage LogItem
              | LogValue Text Integer
                deriving (Generic, Show, ToJSON)

data LogObject = LP LogPrims
               | ObserveOpen CounterState
               | ObserveClose CounterState [LogPrims]
                 deriving (Generic, Show, ToJSON)
\end{code}

\subsubsection{TraceTransformer}\label{code:TraceTransformer}
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

\todo[inline]{TODO |lnName :: Text|\newline storing a concatenation of names
 might be cheaper than rebuilding it for every log message}
\subsubsection{LogNamed}\label{code:LogNamed}
A |LogNamed| contains of a list of context names and some log item.
\begin{code}
type ContextName = Text

-- Attach a 'ContextName' to something.
data LogNamed item = LogNamed
    { lnName :: [ContextName]
    , lnItem :: item
    } deriving (Show)

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

\end{code}

\subsubsection{Observable}\label{code:CounterState}
\begin{code}

type Bytes = Integer

data Counter = MonotonicClockTime Text Microsecond
             | MemoryResidency Text Bytes
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

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}

type TraceContext = MVar TraceController
data TraceController = TraceController {
    traceTransformers :: Map Text TraceTransformer
    }
\end{code}

\subsubsection{TraceConfiguration}\label{code:TraceConfiguration}
\begin{code}

data TraceConfiguration = TraceConfiguration
  { tcOutputKind       :: OutputKind
  , tcName             :: Text
  , tcTraceTransformer :: TraceTransformer
  }

data OutputKind = StdOut | Null deriving Eq

\end{code}
