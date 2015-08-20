{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module TB.Wiki.SmartConstructors (
  Resistor,
  metalResistor,
  ResistorBounded
) where

data Resistor
  = Metal Bands
  | Ceramic Bands
  deriving (Eq, Ord, Show)

type Bands = Int

-- simple smart constructor
metalResistor :: Bands -> Maybe Resistor
metalResistor n
  | n < 4 || n > 8 = Nothing
  | otherwise      = Just $ Metal n


--
-- type level
--

data Z   = Z
data S a = S a

class Cardinal c where

instance Cardinal Z where
instance (Cardinal c) => Cardinal (S c) where

class Cardinal size => InBounds size where

instance InBounds (S (S (S (S Z)))) where                 -- four
instance InBounds (S (S (S (S (S Z))))) where             -- five
instance InBounds (S (S (S (S (S (S Z)))))) where         -- six
instance InBounds (S (S (S (S (S (S (S Z))))))) where     -- seven
instance InBounds (S (S (S (S (S (S (S (S Z)))))))) where -- eight

data ResistorBounded size
  = ResistorBounded
  deriving (Show)

newtype MetalResistorBounded size
  = MetalResistorBounded (ResistorBounded size)
  deriving (Show)

newtype CeramicResistorBounded size
  = CeramicResistorBounded (ResistorBounded size)
  deriving (Show)

resistor :: InBounds size => size -> ResistorBounded size
resistor _ = ResistorBounded

d0  = undefined :: Z
d1  = undefined :: S Z
d2  = undefined :: S (S Z)
d3  = undefined :: S (S (S Z))
d4  = undefined :: S (S (S (S Z)))
d5  = undefined :: S (S (S (S (S Z))))
d6  = undefined :: S (S (S (S (S (S Z)))))
d7  = undefined :: S (S (S (S (S (S (S Z))))))
d8  = undefined :: S (S (S (S (S (S (S (S Z)))))))
d9  = undefined :: S (S (S (S (S (S (S (S (S Z))))))))
d10 = undefined :: S (S (S (S (S (S (S (S (S (S Z)))))))))

metalResistorBounded :: InBounds size => size -> MetalResistorBounded size
metalResistorBounded sz = MetalResistorBounded (resistor sz)

ceramicResistorBounded :: InBounds size => size -> CeramicResistorBounded size
ceramicResistorBounded sz = CeramicResistorBounded (resistor sz)

-- | Examples
--
-- resistor d0
-- Won't compile
--
-- >>> resistord4
-- ResistorBounded
--
-- >>> metalResistorBounded d4
-- MetalResistorBounded ResistorBounded
