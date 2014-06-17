import Control.Applicative

main :: IO ()
main = do
 s <- (++) <$> getLine <*> getLine
 print s
 print $ (+) <$> (+2) <*> (+3) $ 5
 print "applicative"
