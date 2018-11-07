module Cardano.BM.Test.Trace (
    tests
  ) where

import           Cardano.BM.Trace

import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "testing Trace" [
      testProperty "minimal" prop_Trace_minimal
    ]


prop_Trace_minimal :: Bool
prop_Trace_minimal = True

