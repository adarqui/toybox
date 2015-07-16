module TB.GHC.Arr.Examples (
) where

import Control.DeepSeq
import GHC.Arr hiding ((!))
import qualified GHC.Arr as Arr
import Data.Monoid

-- | array
--
-- >>> Arr.array (1,25) $ zip [1..25] [1..25]
-- array (1,25) [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10),(11,11),(12,12),(13,13),(14,14),(15,15),(16,16),(17,17),(18,18),(19,19),(20,20),(21,21),(22,22),(23,23),(24,24),(25,25)]
--
-- >>> (Arr.array (1,25) $ zip [1..25] [1..25]) Arr.! 6
-- 6
--
-- >>> unsafeIndex (0,5) 7
-- 7
--
-- >>> safeIndex (0,5) 3 4
-- *** Exception: Error in array index; 4 not in range [0..3)
--
--

encode :: Int -> (Int,Int) -> Int
{-# INLINE encode #-}
--encode m (i,j) = (i-1)*m + j - 1
encode m (i,j) = (i-1)*m + j

decode :: Int -> Int -> (Int,Int)
{-# INLINE decode #-}
decode m k = (q+1,r+1)
 where
     (q,r) = quotRem k m

data Matrix a = M {
   nrows     :: {-# UNPACK #-} !Int -- ^ Number of rows.
 , ncols     :: {-# UNPACK #-} !Int -- ^ Number of columns.
 , rowOffset :: {-# UNPACK #-} !Int
 , colOffset :: {-# UNPACK #-} !Int
 , vcols     :: {-# UNPACK #-} !Int -- ^ Number of columns of the matrix without offset
 , mvect     :: Array Int a          -- ^ Content of the matrix as a plain vector.
 }

instance Eq a => Eq (Matrix a) where
  m1 == m2 =
    let r = nrows m1
        c = ncols m1
    in  and $ (r == nrows m2) : (c == ncols m2)
            : [ m1 ! (i,j) == m2 ! (i,j) | i <- [1 .. r] , j <- [1 .. c] ]

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: Show a => Matrix a -> String
prettyMatrix m@(M _ _ _ _ _ v) = unlines
 [ "( " <> unwords (fmap (\j -> fill mx $ show $ m ! (i,j)) [1..ncols m]) <> " )" | i <- [1..nrows m] ]
 where
  mx = arrayMaximum $ fmap (length . show) v
  fill k str = replicate (k - length str) ' ' ++ str

instance Show a => Show (Matrix a) where
 show = prettyMatrix

instance NFData a => NFData (Matrix a) where
 rnf = rnf . mvect

instance Functor Matrix where
 {-# INLINE fmap #-}
 fmap f (M n m ro co w v) = M n m ro co w $ amap f v

matrix :: Int -- ^ Rows
       -> Int -- ^ Columns
       -> ((Int,Int) -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m 0 0 m $ array (n, m) elms
  where
    elms =  [ let a = f $ decode m i in (i, a) | i <- [1..n*m] ]

-- | fromList
--
-- >>> fromList 5 5 [1..25]
-- (  1  2  3  4  5 )
-- (  6  7  8  9 10 )
-- ( 11 12 13 14 15 )
-- ( 16 17 18 19 20 )
-- ( 21 22 23 24 25 )
fromList :: Int -- ^ Rows
         -> Int -- ^ Columns
         -> [a] -- ^ List of elements
         -> Matrix a
{-# INLINE fromList #-}
fromList n m vs = M n m 0 0 m $ array (1, n*m) (zip [1..] vs)

-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1,2,3,4,5,6,7,8,9]
--
-- >>> toList $ fromList 5 5 [1..25]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
toList :: Matrix a -> [a]
toList m = [ unsafeGet i j m | i <- [1 .. nrows m] , j <- [1 .. ncols m] ]

-- | arrayMaximum
--
--
arrayMaximum :: Ord a => Array Int a -> a
arrayMaximum m = maximum $ elems m

-- | /O(rows*cols)/. Similar to 'V.force'. It copies the matrix content
--   dropping any extra memory.
--
--   Useful when using 'submatrix' from a big matrix.
--
forceMatrix :: Matrix a -> Matrix a
forceMatrix m = matrix (nrows m) (ncols m) $ \(i,j) -> unsafeGet i j m

-- | Just a cool way to output the size of a matrix.
--
--
sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
getElem :: Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
{-# INLINE getElem #-}
getElem i j m =
  case safeGet i j m of
    Just x -> x
    Nothing -> error
      $ "getElem: Trying to get the "
     ++ show (i,j)
     ++ " element from a "
     ++ sizeStr (nrows m) (ncols m)
     ++ " matrix."

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j (M _ _ ro co w v) = v Arr.! idx
  where
    idx = encode w (i+ro,j+co)

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _ _ _ _)
 | i > n || j > m || i < 1 || j < 1 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | Short alias for 'getElem'.
(!) :: Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | Internal alias for 'unsafeGet'.
(!.) :: Matrix a -> (Int,Int) -> a
{-# INLINE (!.) #-}
m !. (i,j) = unsafeGet i j m
