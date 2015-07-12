module TB.Bifunctor.Examples (
  Point2D (..)
) where

import           Control.Applicative
import           Data.Biapplicative
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.Flip
import           Data.Bitraversable
import           Data.Either
import           Data.Monoid

-- bimap id id ≡ id
-- first id = id
-- second id = id
-- bimap f g = first f . second g
-- bimap (f . g) (h . i) = bimap f h . bimap g i
-- first (f . g) = first f . first g
-- second (f . g) = second f . second g


-- | Tuple
--
-- >>> bimap (+1) (1-) (1,1)
-- (2,0)
--
-- >>> first (+1) (1,1)
-- (2,1)
--
-- >>> second (1-) (1,1)
-- (1,0)


-- | Either
--
-- >>> bimap id (+1) $ Left "error"
-- Left "error"
--
-- >>> bimap id (+1) $ Right 0
-- Right 1



-- data Point2D x y = Point2D x y deriving (Show)
data Point2D x y = Point2D x y deriving (Show)

-- instance Monoid Point2D where
--   mempty = Point2D mempty mempty

instance Bifunctor Point2D where
  bimap f g (Point2D x y) = Point2D (f x) (g y)

instance Biapplicative Point2D where
  bipure = Point2D
  Point2D f g <<*>> Point2D x y = Point2D (f x) (g y)

instance Bifoldable Point2D where
  bifoldMap f g (Point2D x y) = f x

instance Bitraversable Point2D where
  bitraverse f g (Point2D x y) = Point2D <$> (f x) <*> (g y)

-- | Point2D
--
-- >>> bimap (+1) (1-) $ Point2D 1 1
-- Point2D 2 0
--
-- >>> Point2D (+1) (1-) <<*>> Point2D 1 1
-- Point2D 2 0

instance Biapplicative Either where
  bipure _ b = Right b
  Right f <<*>> Right x = Right (f x)
  Left g <<*>> Left x = Left (g x)
