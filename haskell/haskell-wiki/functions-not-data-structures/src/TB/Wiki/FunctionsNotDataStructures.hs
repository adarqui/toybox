module TB.Wiki.FunctionsNotDataStructures (
  FiniteMap,
  empty,
  insert,
  delete,
  lookup
) where

import Prelude hiding (lookup, delete)

type FiniteMap k v = k -> Maybe v

empty :: FiniteMap k v
empty = \k -> Nothing

-- | insert
--
-- >>> lookup 1 (insert 1 "hi" empty)
-- Just "hi"
--
insert :: Eq k => k -> v -> FiniteMap k v -> FiniteMap k v
insert k v m = \k' -> if k == k' then Just v else m k'

delete :: Eq k => k -> FiniteMap k v -> FiniteMap k v
delete k m = \k' -> if k == k' then Nothing else m k'

lookup :: Eq k => k -> FiniteMap k v -> Maybe v
lookup k m = m k
