import Test.QuickCheck

propRev :: [Int] -> Bool
propRev xs = (reverse $ reverse xs) == xs

main :: IO ()
main = do
 quickCheck propRev
 return ()
