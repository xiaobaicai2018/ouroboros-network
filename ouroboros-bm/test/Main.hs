module Main (main) where

import           Test.Tasty

--import qualified Test.STM (tests)
--import qualified Test.Aggregation (tests)
--import qualified Test.Trace (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-bm"
  [ --Test.Trace.tests
  ]
