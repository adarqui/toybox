module TB.WYAH.Parsing.NanoParsec (
  Parser,
  runParser,
  combine,
  failure,
  option,
  satisfy,
  some,
  many,
  item,
  oneOf,
  chainl,
  chainl1,
  char,
  natural,
  string,
  token,
  reserved,
  spaces,
  digit,
  number,
  parens
) where

import           Control.Applicative hiding (many, some)
import           Control.Monad
import           Data.Char

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Parser did not consume entire stream."
    _ -> error "Parser error."

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
    then unit c
    else (Parser $ \cs -> [])

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> (c,cs) : []

-- | One or more
some :: (Applicative f, Alternative f) => f a -> f [a]
some v = (:) <$> v <*> many v

-- | Zero or more
many :: (Applicative f, Alternative f) => f a -> f [a]
many v = some v <|> pure []

{-
many_v :: (Applicative f, Alternative f) => f a -> f [a]
many_v v = some_v v <|> pure []
-}

{-
some_v :: (Applicative f, Alternative f) => f a -> f [a]
some_v v = (:) <$> v <*> many_v v
-}

-- extra

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a = do
        f <- op
        b <- p
        rest (f a b)
      <|>
        return a

--

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = char c >> string cs >> return (c:cs)

token :: Parser a -> Parser a
token p = p >>= \a -> spaces >> return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
