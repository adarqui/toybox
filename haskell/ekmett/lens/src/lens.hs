{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Foo = Foo {
 _string :: String,
 _integer :: Integer,
 _stringList :: [String]
} deriving (Show)

makeLenses ''Foo

main :: IO ()
main = do
 print "len"
