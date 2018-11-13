
\subsection{Trace}

\begin{code}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Test.Trace (
    tests
  ) where

import           Prelude hiding (lookup)

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forM_, void)
import           Data.Map (fromListWith, lookup)
import           Data.Text (Text, append, pack)
import           Data.Set (fromList)

import           Cardano.BM.Data (LogNamed (..), LogObject (ObserveOpen), ObservableInstance (..),
                    OutputKind (..), TraceConfiguration (..), TraceTransformer (..))
import           Cardano.BM.Controller (insertInController, setupTrace, transformTrace)
import           Cardano.BM.STM (bracketObserveIO)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, testCaseInfo)
\end{code}


\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
        testProperty "minimal" prop_Trace_minimal
      , unit_tests
      , testCase "forked Traces stress testing" stress_trace_in_fork
      , testCaseInfo "demonstrating nested named context logging" example_named
      ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
        testCase "opening messages should not be traced" unit_noOpening_Trace
      , testCase "hierarchy testing" unit_hierarchy
      , testCase "forked Traces testing" unit_trace_in_fork
      ]

\end{code}

\begin{code}
prop_Trace_minimal :: Bool
prop_Trace_minimal = True
\end{code}

\begin{code}
-- | example: named context trace
example_named :: IO String
example_named = do
    logTrace <- setupTrace $ TraceConfiguration StdOut "test" Neutral
    putStrLn "\n"
    logInfo logTrace "entering"
    complexWork0 (appendName "simple-work-0" logTrace) "0"
    complexWork1 (appendName "complex-work-1" logTrace) "42"
    -- ^ the named context will include "complex" in the logged message
    logInfo logTrace "done."
    return ""
  where
    complexWork0 tr msg = logInfo tr ("let's see: " `append` msg)
    complexWork1 tr msg = do
        logInfo tr ("let's see: " `append` msg)
        let logTrace' = appendName "inner-work-1" tr
        let observablesSet = fromList [MonotonicClock, MemoryStats]
        insertInController logTrace' "STM-action" (ObservableTrace observablesSet)
        _ <- bracketObserveIO logTrace' "STM-action" setVar_
        logInfo logTrace' "let's see: done."

unit_hierarchy :: Assertion
unit_hierarchy = do
    msgs <- STM.newTVarIO []
    trace0 <- setupTrace $ TraceConfiguration (TVarList msgs) "test" Neutral
    logInfo trace0 "This should have been displayed!"

    -- subtrace of trace which traces nothing
    insertInController trace0 "inner" NoTrace
    (_, trace1) <- transformTrace "inner" trace0
    logInfo trace1 "This should NOT have been displayed!"

    insertInController trace1 "innest" Neutral
    (_, trace2) <- transformTrace "innest" trace1
    logInfo trace2 "This should NOT have been displayed also due to the trace one level above!"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first message should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 1)

unit_trace_in_fork :: Assertion
unit_trace_in_fork = do
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration (TVarListNamed msgs) "test" Neutral
    let trace0 = appendName "work0" trace
    let trace1 = appendName "work1" trace
    void $ forkIO $ work trace0
    threadDelay 500000
    void $ forkIO $ work trace1
    threadDelay (4*second)

    res <- STM.readTVarIO msgs
    let names@(_: namesTail) = map lnName res
    -- each trace should have its own name and log right after the other
    assertBool
        ("Consecutive loggernames are not different: " ++ show names)
        (and $ zipWith (/=) names namesTail)
  where
    work :: Trace IO -> IO ()
    work trace = do
        logInfoDelay trace "1"
        logInfoDelay trace "2"
        logInfoDelay trace "3"
    logInfoDelay :: Trace IO -> Text -> IO ()
    logInfoDelay trace msg =
        logInfo trace msg >>
        threadDelay second

stress_trace_in_fork :: Assertion
stress_trace_in_fork = do
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration (TVarListNamed msgs) "test" Neutral
    let names = map (\a -> ("work-" <> pack (show a))) [1..10]
    forM_ names $ \name -> do
        let trace' = appendName name trace
        void $ forkIO $ work trace'
    threadDelay second

    res <- STM.readTVarIO msgs
    let resNames = map lnName res
    let frequencyMap = fromListWith (+) [(x, 1) | x <- resNames]

    -- each trace should have traced 'totalMessages' messages
    assertBool
        ("Frequencies of logged messages according to loggername: " ++ show frequencyMap)
        (all (\name -> (lookup ["test", name] frequencyMap) == Just totalMessages) names)
  where
    work :: Trace IO -> IO ()
    work trace = forM_ [1..totalMessages] $ (logInfo trace) . pack . show
    totalMessages :: Int
    totalMessages = 10

unit_noOpening_Trace :: Assertion
unit_noOpening_Trace = do
    msgs <- STM.newTVarIO []
    logTrace <- setupTrace $ TraceConfiguration (TVarList msgs) "test" DropOpening
    _ <- bracketObserveIO logTrace "test" setVar_
    res <- STM.readTVarIO msgs
    -- |ObserveOpen| should be eliminated from tracing.
    assertBool
        "Found non-expected ObserveOpen message"
        (all (\case {ObserveOpen _ -> False; _ -> True}) res)

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

second :: Int
second = 1000000

\end{code}
