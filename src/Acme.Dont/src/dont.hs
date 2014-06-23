import Acme.Dont

main :: IO ()
main = don't $ do
 putStrLn "hi."
