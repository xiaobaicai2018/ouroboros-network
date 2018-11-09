
\subsection{\tt{instance} Arbitrary Aggregation}

\begin{code}
module Cardano.BM.Arbitrary.Aggregation

where

import           Test.QuickCheck

import           Cardano.BM.Aggregation
\end{code}

We define an instance of |Arbitrary| for an |Aggregation| which lets |QuickCheck|
generate arbitrary instances of |Aggregation|.
For this an arbitrary list of |Integer| is generated and this list is aggregated into a structure of |Aggregation|.

\begin{code}
instance Arbitrary Aggregation where
    arbitrary = do
        vs' <- arbitrary :: Gen [Integer]
        let delta as = map (uncurry (-)) $ zip as (tail as)
            sum2 = foldr (\e a -> a + e * e) 0
            vs = 42 : 17 : vs'
        return $ Aggregation (Stats (minimum vs) (maximum vs) (toInteger $ length vs) (sum vs) (sum2 vs))
                             (last vs)
                             (Stats (minimum $ delta vs) (maximum $ delta vs) (toInteger $ length vs) (sum $ delta vs) (sum2 $ delta vs))
\end{code}
