import GHC.Exts

showUnboxedInt :: Int# -> String
showUnboxedInt n = (show $ I# n) ++ "#"

main = do
 print $ showUnboxedInt 1
