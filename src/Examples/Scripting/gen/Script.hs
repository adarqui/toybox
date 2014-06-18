module Script (run) where

import Test.QuickCheck

import Props

run :: IO ()

run = do

 putStr "prop_reverse: "
 quickCheck prop_reverse
