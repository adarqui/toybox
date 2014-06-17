import qualified Data.Foldable as F

-- http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-Foldable.html

{-
 instance Foldable [] where
     foldr = Prelude.foldr
     foldl = Prelude.foldl
     foldl' = List.foldl'
     foldr1 = Prelude.foldr1
     foldl1 = Prelude.foldl1

     -- @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
     foldr :: (a -> b -> b) -> b -> t a -> b
     foldr f z t = appEndo (foldMap (Endo . f) t) z
     -- | Right-associative fold of a structure,
     -- but with strict application of the operator.
     foldr' :: (a -> b -> b) -> b -> t a -> b
     foldr' f z0 xs = foldl f' id xs z0
       where f' k x z = k $! f x z
     -- | Left-associative fold of a structure.
     --
     -- @'foldl' f z = 'Prelude.foldl' f z . 'toList'@
     foldl :: (b -> a -> b) -> b -> t a -> b
     foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
     -- | Left-associative fold of a structure.
     -- but with strict application of the operator.
     --
     -- @'foldl' f z = 'List.foldl'' f z . 'toList'@
     foldl' :: (b -> a -> b) -> b -> t a -> b
     foldl' f z0 xs = foldr f' id xs z0
       where f' x k z = k $! f z x
     -- | A variant of 'foldr' that has no base case,
     -- and thus may only be applied to non-empty structures.
     --
     -- @'foldr1' f = 'Prelude.foldr1' f . 'toList'@
     foldr1 :: (a -> a -> a) -> t a -> a
     foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                     (foldr mf Nothing xs)
       where
         mf x Nothing = Just x
         mf x (Just y) = Just (f x y)
     -- | A variant of 'foldl' that has no base case,
     -- and thus may only be applied to non-empty structures.
     --
     -- @'foldl1' f = 'Prelude.foldl1' f . 'toList'@
     foldl1 :: (a -> a -> a) -> t a -> a
     foldl1 f xs = fromMaybe (error "foldl1: empty structure")
                     (foldl mf Nothing xs)

-}
main :: IO ()
main = do
 print "foldable"
 print $ F.foldl (\b a -> a : b) [] [1,2,3,4]
 print $ F.foldl' (\b a -> a : b) [] [1,2,3,4]
 print $ F.foldr (\a b -> a : b) [] [1,2,3,4]
