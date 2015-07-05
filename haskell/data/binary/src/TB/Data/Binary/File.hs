{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TB.Data.Binary.File (
  Person (..),
  encodePerson,
  decodePerson,

  Person' (..),
  encodePerson',
  decodePerson',

  Person'' (..),
  encodePerson'',
  decodePerson''
) where

import           Control.Exception
import           Control.Monad
import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           System.IO

-- $setup
-- >>> :set -XOverloadedStrings

------------------------------------------------------------------------------------------------------------------------

-- | Normal record
data Person = Person {
  alive     :: Bool,
  firstName :: Text,
  lastName  :: Text
} deriving (Eq, Show, Generic)

-- | Automatic instance via Generic
--
-- >>> decode (encode $ Person True "Andrew" "Darqui") :: Person
-- Person {alive = True, firstName = "Andrew", lastName = "Darqui"}
instance Binary Person

-- | Encode Person to a file
--
-- >>> encodePerson "/tmp/binary.txt" $ Person True "Andrew" "Darqui"
-- Right ()
encodePerson :: FilePath -> Person -> IO (Either Text ())
encodePerson path person = do
  encodeFile path person
  return $ Right ()

-- | Decode Person from a file
--
-- >>> decodePerson "/tmp/binary.txt"
-- Right (Person {alive = True, firstName = "Andrew", lastName = "Darqui"})
decodePerson :: FilePath -> IO (Either Text Person)
decodePerson path = do
  onException
    (decodeFile path >>= return . Right)
    (return $ Left "decode error")

------------------------------------------------------------------------------------------------------------------------

-- | Manual Person encoding/decoding
data Person' = Person' {
  alive'     :: Bool,
  firstName' :: Text,
  lastName'  :: Text
} deriving (Eq, Show)

-- | Manual Person encoding/decoding
--
-- >>> decode (encode $ Person' True "Andrew" "Darqui") :: Person'
-- Person' {alive' = True, firstName' = "Andrew", lastName' = "Darqui"}
instance Binary Person' where
  put p = do
    put $ alive' p
    put $ firstName' p
    put $ lastName' p

  get = do
    _alive <- get :: Get Bool
    _firstName <- get :: Get Text
    _lastName <- get :: Get Text
    return Person' {
      alive' = _alive,
      firstName' = _firstName,
      lastName' = _lastName
    }

-- >>> encodePerson' "/tmp/binary.txt" $ Person' True "Andrew" "Darqui"
-- Right ()
encodePerson' :: FilePath -> Person' -> IO (Either Text ())
encodePerson' path person = do
  encodeFile path person
  return $ Right ()
  {-
  withFile path WriteMode (\h -> L.hPutStr h $ encode person)
  return $ Right ()
  -}

-- >>> decodePerson' "/tmp/binary.txt"
-- Right (Person' {alive' = True, firstName' = "Andrew", lastName' = "Darqui"})
decodePerson' :: FilePath -> IO (Either Text Person')
decodePerson' path = do
  decodeFile path >>= return . Right
  {-
  withFile path ReadMode (\h -> L.hGetContents h >>= return . Right . decode)
  -}

------------------------------------------------------------------------------------------------------------------------

-- | Manual Person encoding/decoding using liftM
data Person'' = Person'' {
  alive''     :: Bool,
  firstName'' :: Text,
  lastName''  :: Text
} deriving (Eq, Show)

-- | Manual Person encoding/decoding using liftM
-- >>> decode (encode $ Person'' True "Andrew" "Darqui") :: Person''
-- Person'' {alive'' = True, firstName'' = "Andrew", lastName'' = "Darqui"}
instance Binary Person'' where
  put Person''{..} = do
    liftM3 (\_ _ _ -> ()) (put alive'')  (put firstName'') (put lastName'')

  get = do
    liftM3 Person'' get get get

-- | Encode a Person''
--
-- >>> encodePerson'' "/tmp/binary.txt" $ Person'' True "Andrew" "Darqui"
-- Right ()
encodePerson'' :: FilePath -> Person'' -> IO (Either Text ())
encodePerson'' path person = do
  encodeFile path person
  return $ Right ()

-- | Decode to a Person''
--
-- >>> decodePerson'' "/tmp/binary.txt"
-- Right (Person'' {alive'' = True, firstName'' = "Andrew", lastName'' = "Darqui"})
decodePerson'' :: FilePath -> IO (Either Text Person'')
decodePerson'' path = do
  decodeFile path >>= return . Right
