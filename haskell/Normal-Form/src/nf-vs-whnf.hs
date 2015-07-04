import System.Time
import Control.Concurrent
import Control.Parallel

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

main :: IO ()
main = do
 elapsed "Printing nf-vs-whnf" (print "nf-vs-whnf") >>= print . show

 elapsed "threadDelay 1000000" (threadDelay 1000000) >>= print . show

 elapsed "par [1..] return ()" ([1..] `par` return ()) >>= print . show

 elapsed "pseq (foldl (+1) 0 [1..10000000]) return ())" ((foldl (+) 0 [1..10000000]) `pseq` return ()) >>= print . show
 elapsed "par (foldl (+1) 0 [1..10000000]) return ())" ((foldl (+) 0 [1..10000000]) `par` return ()) >>= print . show
