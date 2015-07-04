import System.IO
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import qualified Data.Map as M

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addEvent = do
   eKey <- fromAddHandler addEvent
   eCnt <- fromAddHandler addEvent
   eMap <- fromAddHandler addEvent
   eChars <- fromAddHandler addEvent
   let
    eKeyChange = eKey
    bKey = stepper 'A' eKeyChange
    eCounter = accumE 0 ((+1) <$ eCnt)
    bCnt = stepper 0 eCounter
--    bMap = stepper
--    eCharsChange = collect eKey
    eCharsChange = accumE [] ((\x -> x ++ ".") <$ eKey)
--    bChar = stepper ['A'] eCharsChange
    bChar = accumB [] ((\x -> x ++ "b") <$ eCharsChange)
   eKeyChanged <- changes bKey
   eCntChanged <- changes bCnt
   eCharsChanged <- changes bChar
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eKeyChanged
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eCntChanged
   reactimate' $ fmap (\n -> putStrLn $ show n) <$> eCharsChanged

main :: IO ()
main = do
 (addKeyEvent, fireKey) <- newAddHandler
 network <- compile $ makeNetworkDescription addKeyEvent
 actuate network
 hSetEcho stdin False
 hSetBuffering stdin NoBuffering
 forever (getChar >>= fireKey)
