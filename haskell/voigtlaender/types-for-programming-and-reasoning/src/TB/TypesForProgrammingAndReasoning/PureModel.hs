{-# LANGUAGE RankNTypes #-}

module TB.TypesForProgrammingAndReasoning.PureModel (
) where

import Prelude hiding (getChar, putChar)

data IOSpec a
  = Return a
  | GetChar (Char -> IOSpec a)
  | PutChar Char (IOSpec a)

data Trace a
  = Finish a
  | Read Char (Trace a)
  | Write Char (Trace a)
  deriving (Show)

instance Monad IOSpec where
  return              = Return
  (Return a) >>= k    = k a
  (GetChar f) >>= k   = GetChar (\c -> (f c) >>= k)
  (PutChar c m) >>= k = PutChar c (m >>= k)

getChar :: IOSpec Char
getChar = GetChar Return

putChar :: Char -> IOSpec ()
putChar c  = PutChar c (Return ())

trace :: forall a. IOSpec a -> String -> Trace a
trace (Return a) cs      = Finish a
trace (GetChar f) (c:cs) = Read c (trace (f c) cs)
trace (PutChar c m) cs   = Write c (trace m cs)

echo :: IOSpec ()
echo = getChar >>= (\c -> if c /= '*' then echo >>= (\() -> putChar c)
                                      else return ())

-- | stuff
--
-- >>> trace echo "ab*"
-- Read 'a' (Read 'b' (Read '*' (Write 'b' (Write 'a' (Finish ())))))
