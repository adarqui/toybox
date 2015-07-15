module TB.Useless.Permutations (
  carvePerms,
  dupPerms
) where

-- | carvePerms
--
-- >>> carvePerms 4 [0,1]
-- [[0],[1],[0,0],[0,1],[1,0],[1,1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1],[0,0,0,0],[0,0,0,1],[0,0,1,0],[0,0,1,1],[0,1,0,0],[0,1,0,1],[0,1,1,0],[0,1,1,1],[1,0,0,0],[1,0,0,1],[1,0,1,0],[1,0,1,1],[1,1,0,0],[1,1,0,1],[1,1,1,0],[1,1,1,1]]
carvePerms :: Int -> [a] -> [[a]]
carvePerms n codes = fst $ break (\xs -> length xs > n) $ dupPerms codes

dupPerms :: [a] -> [[a]]
dupPerms xs = dupPerms' xs [[]]

dupPerms' :: [a] -> [[a]] -> [[a]]
dupPerms' [] _ = []
dupPerms' codes acc = concated ++ dupPerms' codes concated
  where
    concated = [ xs : ys | xs <- codes, ys <- acc ]
