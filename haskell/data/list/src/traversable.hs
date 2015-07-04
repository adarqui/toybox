-- https://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Traversable.html

import qualified Data.Traversable as T

{-
 instance Traversable [] where
     {-# INLINE traverse #-} -- so that traverse can fuse
     traverse f = Prelude.foldr cons_f (pure [])
       where cons_f x ys = (:) <$> f x <*> ys
     mapM = Prelude.mapM

foldr (\x ys -> (:) <$> fx <*> yx) (pure [])
-}

main :: IO ()
main = do
 print "traversable"
 T.traverse (print) [1,2,3,4]
 T.mapM print ["hi","!"]
 return ()
