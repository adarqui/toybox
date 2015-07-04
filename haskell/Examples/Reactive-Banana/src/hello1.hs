import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
 putStrLn "hello1"
 (addHandler, fire) <- newAddHandler
 register addHandler (\x -> (print "1" >> putStrLn x))
 register addHandler (\x -> (print "2" >> print x))
 register addHandler (\x -> (print "3" >> (((print . reverse) x))))
 fire "hello"
 fire $ reverse "hello"
