module TB.Data.Function.Fix.Examples (
  countFromTo,
  countToFrom,
  factorial,
  fibonacci
) where

import           Data.Function

fix' :: (a -> a) -> a
fix' = \f -> let x = f x in x

fix'' :: (a -> a) -> a
fix'' f = x
    where x = f x

-- | Count from x to y
--
-- >>> countFromTo 1 10
-- 10
countFromTo =
  fix' (\f -> \from -> \to ->
    if from == to
       then to
       else f (from+1) to)

-- | Count down from y to x
--
-- >>> countToFrom 0 10
-- 0
countToFrom =
  fix'' (\f -> \to -> \from ->
    if from == to
       then to
       else f to (from-1))

-- | factorial using fix
--
-- >>> factorial 5
-- 120
factorial =
  fix (\fact -> \x ->
      case x of
        0 -> 1
        _ -> x * (fact (x - 1)))

-- | fibonacci using fix
--
-- >>> fibonacci 5
-- 5
--
-- >>> fibonacci 10
-- 55
fibonacci =
  fix (\fib -> \x ->
    case x of
      0 -> 0
      1 -> 1
      _ -> fib (x - 1) + fib (x - 2))
