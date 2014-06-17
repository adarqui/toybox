{-# LANGUAGE OverloadedStrings #-}

import Pipes.Csv (HasHeader(..), decode, decodeByName)
import Pipes.ByteString (stdin, ByteString)
import Data.Csv ((.:), FromNamedRecord(..), Record)
import Pipes
import Control.Applicative

import System.Environment
 (getArgs)

data Person = Person String Int
            deriving (Show)

instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "age"

persons :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
persons = decodeByName

collect :: Monad m => Producer ByteString m () -> Producer (Either String [String]) m ()
collect = decode NoHeader

usage :: String
usage = "usage: ./stdinToStdout <index>"

main = do
 argv <- getArgs
 case argv of
  ("1":[]) -> runEffect $ for (persons stdin) (lift . print)
  ("2":[]) -> runEffect $ for (collect stdin) (lift . print)
{-  ("1":[]) -> runEffect $ for (persons stdin) (lift . \_ -> return ())
  ("2":[]) -> runEffect $ for (collect stdin) (lift . \_ -> return ())
  -}
  _ -> putStrLn usage
