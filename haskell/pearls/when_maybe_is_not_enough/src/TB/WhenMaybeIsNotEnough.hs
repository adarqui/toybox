module TB.WhenMaybeIsNotEnough (
) where

import Control.Applicative
import Control.Monad

type Parser1 a = String -> [(a, String)]
type Parser2 a = String -> Maybe (a, String)

newtype ParserF a = ParserF { runParserF :: String -> (a, String) }
newtype Parser m a = Parser { runParser :: String -> m (a, String) }

instance Functor ParserF where
  fmap f (ParserF p) = ParserF $ \s -> let (a', s') = p s in (f a', s')

-- | eh
--
-- >>> runParserF (t1 5) "hello"
-- (5,"hello")
t1 :: Int -> ParserF Int
t1 n = ParserF (\s -> (n, s))

{-
instance Applicative (Parser m) where
  pure x = Parser (\s -> (x, s))
-}

instance Monad m => Monad (Parser m) where
  return x = Parser (\s -> return (x, s))
  xp >>= f = Parser (\s -> runParser xp s >>= (\(x, s') -> runParser (f x) s'))

instance MonadPlus m => MonadPlus (Parser m) where
  mzero = Parser (\s -> mzero)
  xp `mplus` yp = Parser (\s -> runParser xp s `mplus` runParser yp s)

-- | alt aka msum
alt :: MonadPlus m => [m a] -> m a
alt = foldr mplus mzero

-- | pChar
--
-- >>> runParser ((pChar 'x' >> pChar 'x') `mplus` (pChar 'x' >> pChar 'y'))"xy" :: Maybe ((), String)
-- Just ((),"")
pChar :: MonadPlus m => Char -> Parser m ()
pChar c =
  Parser $ \s ->
    case s of
      (c':s') | c == c' -> return ((), s')
      _ -> mzero

-- | pString
--
-- >>> runParser (pString "x" `mplus` pString "xx") "xxy" :: Maybe ((), String)
-- Just ((),"xy")
pString :: MonadPlus m => String -> Parser m ()
pString = foldr (>>) (return ()) . map pChar


-- Post's correspondence problem

type Tile = (String, String)

tiles1 = [("b", "ca"), ("a", "ab"), ("ca", "a"), ("abc", "c")]

correct :: [Tile] -> Bool
correct layout = (concat (map fst layout) == concat (map snd layout))

-- | choices
--
-- >>> take 10 $ choices 4
-- [[],[0],[1],[2],[3],[0,0],[0,1],[0,2],[0,3],[1,0]]
choices :: Int -> [[Int]]
choices n = concat (iterate step [[]])
  where
    step css = [ c : cs | c <- [0..n-1], cs <- css]

assembly :: MonadPlus m => [String] -> Parser m ()
assembly tags = p where
  p = alt [(pString (show i) >> ((pChar ',' >> p) `mplus` pChar '=') >> pString tag) | (i, tag) <- zip [0..] tags]

ambiguous :: [Tile] -> Parser [] Int
ambiguous tiles =
  (assembly (map fst tiles) >> pChar '!' >> return 1)
  `mplus`
  (assembly (map snd tiles) >> pChar '!' >> return 2)

-- | examples
--
-- >>> runParser (ambiguous tiles1) "3,1=aabc!"
--

{-
solutions :: [Tile] -> [[Tile]]
solutions tiles =
  [ cs | cs <- tail (choices (length tiles)), correct (map (tiles!!) cs) ]
-}
