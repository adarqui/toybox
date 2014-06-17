-- https://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-Typeable.html

import Data.Typeable
import Data.Data
import Data.Maybe

main :: IO ()
main = do
 print "typeable"
 print $ dataTypeConstrs $ dataTypeOf $ Just 'a'
