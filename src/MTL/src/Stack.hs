module Stack (
 pop,
 push,
 (^),
 v,
 enqueue,
 dequeue,
 (^^),
 vv
) where


import Control.Monad
import Control.Monad.State
import Prelude hiding ((^), (^^))

pop :: State [a] a
pop = do
 x:xs <- get
 put xs
 return x


dequeue :: State [a] a
dequeue = do
 xl <- get
 put $ init xl
 return $ last xl


push :: a -> State [a] ()
push a = do
 xs <- get
 put (a:xs)
 return ()


enqueue :: a -> State [a] ()
enqueue a = do
 xl <- get
 put (xl ++ [a])
 return ()


(^) :: a -> State [a] ()
(^) = push


v :: State [a] a
v = pop


(^^) :: a -> State [a] ()
(^^) = enqueue


vv :: State [a] a
vv = dequeue
