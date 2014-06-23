{-----------------------------------------------------------------------------
    reactive-banana
   
    Example: Actuate and pause an event network
------------------------------------------------------------------------------}
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana
import Reactive.Banana.Frameworks

{-
 -- | Build a facility to register and unregister event handlers.
 -- Also yields a function that takes an event handler and runs all the registered
 -- handlers.
 --
 -- Example:
 --
 -- > do
 -- >     (addHandler, fire) <- newAddHandler
 -- >     register addHandler putStrLn
 -- >     fire "Hello!"
 newAddHandler :: IO (AddHandler a, Handler a)
 newAddHandler = do
     handlers <- newIORef Map.empty
     let register handler = do
             key <- Data.Unique.newUnique
             atomicModifyIORef_ handlers $ Map.insert key handler
             return $ atomicModifyIORef_ handlers $ Map.delete key
         runHandlers a =
             mapM_ ($ a) . map snd . Map.toList =<< readIORef handlers
     return (AddHandler register, runHandlers)
 atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
-}

{-
 -- | Actuate an event network.
 -- The inputs will register their event handlers, so that
 -- the networks starts to produce outputs in response to input events.
 actuate :: EventNetwork -> IO ()
 actuate = Prim.actuate . unEN
-}

main :: IO ()
main = do
    displayHelpMessage
    sources <- (,) <$> newAddHandler <*> newAddHandler
    network <- setupNetwork sources
    actuate network
    eventLoop sources network

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "Commands are:":
    "   count   - send counter event":
    "   pause   - pause event network":
    "   actuate - actuate event network":
    "   quit    - quit the program":
    "":
    []

-- Read commands and fire corresponding events
eventLoop :: (EventSource (),EventSource EventNetwork) -> EventNetwork -> IO ()
eventLoop (escounter, espause) network = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        case s of
            "count"   -> fire escounter ()
            "pause"   -> fire espause network
            "actuate" -> actuate network
            "quit"    -> return ()
            _         -> putStrLn $ s ++ " - unknown command"
        when (s /= "quit") loop

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
-- Set up the program logic in terms of events and behaviors.

{-
 -- | Compile the description of an event network
 -- into an 'EventNetwork'
 -- that you can 'actuate', 'pause' and so on.
 --
 -- Event networks are described in the 'Moment' monad
 -- and use the 'Frameworks' class constraint.
 compile :: (forall t. Frameworks t => Moment t ()) -> IO EventNetwork
 compile m = fmap EN $ Prim.compile $ unM (m :: Moment (FrameworksD, t) ())
-}

{-
 Note: An event network essentially behaves like a single,
 huge callback function. The 'IO' action are not run in a separate thread.
 The callback function will throw an exception if one of your 'IO' actions
 does so as well.
 Your event-based framework will have to handle this situation.
 reactimate :: Frameworks t => Event t (IO ()) -> Moment t ()
 reactimate = M . Prim.addReactimate . Prim.mapE (return . sequence_) . unE
 -- | Output.
 -- Execute the 'IO' action whenever the event occurs.
 --
 -- This version of 'reactimate' can deal with values obtained
 -- from the 'changes' function.
 reactimate' :: Frameworks t => Event t (Future (IO ())) -> Moment t ()
 reactimate' = M . Prim.addReactimate . Prim.mapE (unF . fmap sequence_ . sequence)
 -- | Input,
 -- obtain an 'Event' from an 'AddHandler'.
 --
 -- When the event network is actuated,
 -- this will register a callback function such that
 -- an event will occur whenever the callback function is called.
 fromAddHandler :: Frameworks t => AddHandler a -> Moment t (Event t a)
 fromAddHandler = M . fmap singletonsE . Prim.fromAddHandler
-}

setupNetwork :: (EventSource (),EventSource EventNetwork) -> IO EventNetwork
setupNetwork (escounter, espause) = compile $ do
    ecounter <- fromAddHandler (addHandler escounter)
    epause   <- fromAddHandler (addHandler espause  )
   
    let ecount = accumE 0 ((+1) <$ ecounter)
   
    reactimate $ fmap print ecount
    reactimate $ fmap pause epause

{-
 -- | Pause an event network.
 -- Immediately stop producing output and
 -- unregister all event handlers for inputs.
 -- Hence, the network stops responding to input events,
 -- but it's state will be preserved.
 --
 -- You can resume the network with 'actuate'.
 --
 -- Note: You can stop a network even while it is processing events,
 -- i.e. you can use 'pause' as an argument to 'reactimate'.
 -- The network will /not/ stop immediately though, only after
 -- the current event has been processed completely.
 pause :: EventNetwork -> IO ()
 pause   = Prim.pause . unEN
-}
