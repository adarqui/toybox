{-# LANGUAGE RecordWildCards #-}

-- https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/syntax-extns.html

module TB.Language.Extensions.RecordWildCards (
  Person (..),
  Gender,
  newPerson,
  fullName,
  kill
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude   hiding (last)

data Gender = M | F deriving (Eq, Show)

data Person = Person {
  alive     :: Bool,
  firstName :: Text,
  lastName  :: Text,
  age       :: Int,
  gender    :: Gender
} deriving (Show)

newPerson :: Text -> Text -> Gender -> Person
newPerson first last sex =
  let
    alive = True
    firstName = first
    lastName = last
    age = 0
    gender = sex
  in Person{..}

{-
newPersonIO :: Text -> Text -> Gender -> Person
newPersonIO first last sex = do
  alive <- True
  firstName <- first
  lastName <- last
  age <- 0
  gender <- sex
  return Person{..}
-}

fullName :: Person -> Text
fullName Person{..} = firstName `T.append` " " `T.append` lastName

-- Just because.
kill :: Person -> Person
kill p =
  p { alive = False }
