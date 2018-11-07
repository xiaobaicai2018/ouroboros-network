module Cardano.BM.Test.STM (
    tests
  ) where

import           Cardano.BM.STM

import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "observing STM actions" [
      testProperty "minimal" prop_STM_observer
    ]


prop_STM_observer :: Bool
prop_STM_observer = True

