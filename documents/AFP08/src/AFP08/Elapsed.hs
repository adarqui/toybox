module AFP08.Elapsed (
 secsDiff,
 elapsed,
 elapsed'Show
) where

import System.Time
 (ClockTime(..), getClockTime)

import Control.Parallel
 (par, pseq)

secsDiff :: ClockTime -> ClockTime -> Float
secsDiff (TOD secs1 psecs1) (TOD secs2 psecs2) =
 fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

elapsed :: String -> IO () -> IO Float
elapsed desc fn = do
 putStrLn ("Starting: " ++ desc)
 t0 <- getClockTime
 fn
 t1 <- getClockTime
 return $ secsDiff t0 t1

elapsed'Show :: String -> IO () -> IO ()
elapsed'Show desc fn = do
 float <- elapsed desc fn
 putStrLn $ "Elapsed time: " ++ show float
