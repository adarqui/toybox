module TB.Statistics.Examples (
) where

import Data.Vector
import Statistics.Sample

v1 = fromList [1,1,1,4,8,10,12,9,7,5,3,1,1,1,2,1]
weights = fromList [0.1,0.1,0.1,0.4,0.8,1,1.2,0.9,0.7,0.5,0.3,0.1,0.1,0.1,0.1,0.2,0.1]

stats =
  [
    ("stdDev", stdDev v1)
  , ("skewness", skewness v1)
  , ("range", range v1)
  , ("mean", mean v1)
  , ("welfordMean", welfordMean v1)
  , ("meanWeighted", meanWeighted $ Data.Vector.zip v1 weights)
  , ("harmonicMean", harmonicMean v1)
  , ("geometricMean", geometricMean v1)
  , ("kurtosis", kurtosis v1)
  , ("variance", variance v1)
  , ("varianceUnbiased", varianceUnbiased v1)
  , ("meanVariance fst", fst mv)
  , ("meanVariance snd", snd mv)
  ]
  where
    mv = meanVariance v1
