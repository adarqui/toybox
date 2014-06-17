import GWL

import System.Environment

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (g:w:l:[]) -> putStrLn $ show $ findStableForests (Forest {goats = ri g, wolves = ri w, lions = ri l})
  _ -> usage
 where
  usage = putStrLn "./gwl goats wolves lions"
  ri i = read i :: Int

