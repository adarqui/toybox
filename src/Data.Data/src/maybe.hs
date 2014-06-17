import Data.Typeable
import Data.Data

main :: IO ()
main = do
 print $ dataTypeOf $ Just (1 :: Integer)
 print $ dataTypeConstrs $ dataTypeOf $ Just "Hi"
 print $ isAlgType $ dataTypeOf $ Just (1 :: Integer)
 print $ isAlgType $ dataTypeOf (1 :: Integer)
