module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Aggregation (tests)
import qualified Cardano.BM.Test.STM (tests)
import qualified Cardano.BM.Test.Trace (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-bm"
  [ Cardano.BM.Test.Aggregation.tests
  , Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  ]
