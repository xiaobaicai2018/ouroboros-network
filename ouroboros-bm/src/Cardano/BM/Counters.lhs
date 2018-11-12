
\subsection{Cardano.BM.Counters}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Counters
    (
      readCounters
    ) where

import           Data.Foldable (foldrM)
import           Data.Set (member)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Word (Word64)

import           Cardano.BM.Data (Counter (..), ObservableInstance (..),
                     TraceTransformer (..))
\end{code}
%endif

\begin{code}
nominalDiffTimeToMicroseconds :: Word64 -> Microsecond
nominalDiffTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{code}

\todo[inline]{we have to expand the |getMonoClock| and |readMemStats| functions\newline with ones that read full data from '/proc/'}
\begin{code}

readCounters :: TraceTransformer -> IO [Counter]
readCounters NoTrace       = return []
readCounters Neutral       = return []
readCounters UntimedTrace  = return []
readCounters DropOpening   = return []
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

