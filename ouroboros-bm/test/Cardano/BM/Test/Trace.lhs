
\subsection{Trace}

\begin{code}
module Cardano.BM.Test.Trace (
    tests
  ) where

import           Control.Monad (when)
import           Data.Text (append)

import           Cardano.BM.Trace (OutputKind (StdOut), TraceConfiguration (..),
                    TraceTransformer (Neutral), appendName, logInfo, setupTrace)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.HUnit (testCaseInfo)


tests :: TestTree
tests = testGroup "testing Trace" [
      testProperty "minimal" prop_Trace_minimal
    , testCaseInfo "demonstrating nested named context logging" example_named
    ]

prop_Trace_minimal :: Bool
prop_Trace_minimal = True

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
        when (msg == "42") $
                complexWork1 (appendName "inner-work-1" tr) "done."
\end{code}
