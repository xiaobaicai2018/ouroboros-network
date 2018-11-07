
module Cardano.BM.Aggregation
  (
    Aggregation (..)
  , Stats (..)
  , updateAggregation
  ) where


data Stats = Stats {
    fmin :: Integer,
    fmax :: Integer,
    fcount :: Integer,
    fsumA :: Integer,
    fsumB :: Integer
    } deriving (Show, Eq)

data Aggregation = Aggregation {
    fstats :: Stats,
    flast :: Integer,
    fdelta :: Stats
    } deriving (Show, Eq)

updateAggregation :: Integer -> Maybe Aggregation -> Maybe Aggregation
updateAggregation v Nothing =
    Just $
    Aggregation { fstats = Stats {
                    fmin=v , fmax=v , fcount=1
                  , fsumA=v , fsumB=v * v }
                , flast = v
                , fdelta = Stats {
                    fmin=0 , fmax=0 , fcount=0
                  , fsumA=0 , fsumB=0 }
                }
updateAggregation v (Just (Aggregation (Stats _min _max _count _sumA _sumB)
                                       _last
                                       (Stats _dmin _dmax _dcount _dsumA _dsumB)
                          )) =
    let delta = v - _last
    in
    Just $
    Aggregation { fstats = Stats {
                      fmin=(min _min v) , fmax=(max _max v)
                    , fcount=(_count + 1)
                    , fsumA=(_sumA + v) , fsumB=(_sumB + v * v) }
                , flast = v
                , fdelta = Stats {
                    fmin=(min _dmin delta), fmax=(max _dmax delta)
                  , fcount=(_dcount + 1)
                  , fsumA=(_dsumA + delta) , fsumB=(_dsumB + delta * delta) }
                }

