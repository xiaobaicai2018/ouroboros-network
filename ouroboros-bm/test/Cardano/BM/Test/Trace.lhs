
\subsection{Trace}

\begin{code}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Test.Trace (
    tests
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Data.Text (append)
import           Data.Set (fromList)

import           Cardano.BM.Data (LogObject (ObserveOpen), ObservableInstance (..),
                    OutputKind (StdOut), TraceConfiguration (..),
                    TraceTransformer (..))
import           Cardano.BM.Controller (insertInController, setupTrace)
import           Cardano.BM.STM (bracketObserveIO)
import           Cardano.BM.Trace (appendName, logInfo, traceInTVarIO)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Property, ioProperty, testProperty)
import           Test.Tasty.HUnit (testCaseInfo)
\end{code}


\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
      testProperty "minimal" prop_Trace_minimal
    , testProperty "opening messages should not be traced" prop_noOpening_Trace
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

prop_noOpening_Trace :: Property
prop_noOpening_Trace = ioProperty $ do
    (ctx, _) <- setupTrace $ TraceConfiguration StdOut "test" DropOpening
    msgs <- STM.newTVarIO []
    let tVarTrace = traceInTVarIO msgs
    _ <- bracketObserveIO (ctx, tVarTrace) "test" setVar_
    res <- STM.readTVarIO msgs
    putStrLn $ show res
    -- |ObserveOpen| should be eliminated from tracing.
    return $ and $ map (\case {ObserveOpen _ -> False; _ -> True}) res

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res
\end{code}
