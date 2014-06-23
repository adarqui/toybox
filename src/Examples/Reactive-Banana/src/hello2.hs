import System.IO
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

data Keys = Keys Char Char deriving (Show)

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addEvent = do
   eKey <- fromAddHandler $ addEvent
   eCnt <- fromAddHandler $ addEvent
   let
    eKeyChange = filterE (\c -> c /= '\n') eKey
    eLine = filterE (\c -> c == '\n') eKey
    bKey = stepper 'A' eKeyChange
    bLine = stepper 'A' eLine
    bKeys = Keys <$> bKey <*> bLine
    eCounter = accumE 0 ((+1) <$ eCnt)
    bCnt = stepper 0 eCounter
   eKeyChanged <- changes bKeys
   eCntChanged <- changes bCnt
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eKeyChanged
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eKeyChanged
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eCntChanged

main :: IO ()
main = do
 putStrLn "hello2"
 (addKeyEvent, fireKey) <- newAddHandler
 network <- compile $ makeNetworkDescription addKeyEvent
 actuate network
 hSetBuffering stdin NoBuffering
 forever $ do
  c <- getChar
  case c of
   'p' -> pause network
   'r' -> actuate network
   'q' -> return ()
   _ -> fireKey c
  return ()
