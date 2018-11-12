
\subsection{Trace}

\begin{code}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Test.Trace (
    tests
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (void)
import           Data.Text (append)
import           Data.Set (fromList)

import           Cardano.BM.Data (LogNamed (..), LogObject (ObserveOpen), ObservableInstance (..),
                    OutputKind (..), TraceConfiguration (..), TraceTransformer (..))
import           Cardano.BM.Controller (insertInController, setupTrace, transformTrace)
import           Cardano.BM.STM (bracketObserveIO)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Property, ioProperty, testProperty)
import           Test.Tasty.HUnit (testCaseInfo)
\end{code}


\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
      testProperty "minimal" prop_Trace_minimal
    , testProperty "opening messages should not be traced" prop_noOpening_Trace
    , testProperty "hierarchy testing" prop_hierarchy
    , testProperty "forked Traces testing" prop_trace_in_fork
    , testCaseInfo "demonstrating nested named context logging" example_named
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

prop_hierarchy :: Property
prop_hierarchy = ioProperty $ do
    msgs <- STM.newTVarIO []
    trace0 <- setupTrace $ TraceConfiguration StdOut {-(TVarList msgs)-} "test" Neutral
    logInfo trace0 "This should have been displayed!"

    -- subtrace of trace which traces nothing
    (_, trace1) <- transformTrace "inner" trace0
    -- insertInController trace1 "inner" Neutral --NoTrace
    logInfo trace1 "This should NOT have been displayed!"

    -- let subtrace' = appendName "innest" trace1
    -- insertInController subtrace' "innest" Neutral
    -- (_, trace2) <- transformTrace "innest" subtrace'
    -- logInfo trace2 "This should not have been displayed also since!"

    -- res <- STM.readTVarIO msgs
    -- putStrLn $ show res

    return $ False --length res == 1

prop_trace_in_fork :: Property
prop_trace_in_fork = ioProperty $ do
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration (TVarListNamed msgs) "test" DropOpening
    -- trace <- setupTrace $ TraceConfiguration StdOut "test" Neutral
    let trace0 = appendName "work0" trace
    let trace1 = appendName "work1" trace
    void $ forkIO $ work0 trace0
    threadDelay 500000
    void $ forkIO $ work1 trace1
    threadDelay 3500000
    res <- STM.readTVarIO msgs
    let names@(_: namesTail) = map lnName res
    putStrLn $ show $ names
    return $ and $ zipWith (/=) names namesTail
  where
    work0 :: Trace IO -> IO ()
    work0 trace = do
        logInfo trace "1"
        threadDelay 1000000
        logInfo trace "2"
        threadDelay 1000000
        logInfo trace "3"
        threadDelay 1000000
    work1 :: Trace IO -> IO ()
    work1 trace = do
        logInfo trace "a"
        threadDelay 1000000
        logInfo trace "b"
        threadDelay 1000000
        logInfo trace "c"
        threadDelay 1000000

prop_noOpening_Trace :: Property
prop_noOpening_Trace = ioProperty $ do
    msgs <- STM.newTVarIO []
    logTrace <- setupTrace $ TraceConfiguration (TVarList msgs) "test" DropOpening
    _ <- bracketObserveIO logTrace "test" setVar_
    res <- STM.readTVarIO msgs
    -- |ObserveOpen| should be eliminated from tracing.
    return $ all (\case {ObserveOpen _ -> False; _ -> True}) res

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res
\end{code}
