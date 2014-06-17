module StackT (
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

import qualified Stack as S

runStack p = do
 st <- get
 let (a,s) = runState p st
 put s
 return a

pop :: StateT [a] IO a
pop = runStack S.pop

dequeue :: StateT [a] IO a
dequeue = runStack S.dequeue

push :: a -> StateT [a] IO ()
push a = runStack (S.push a)

enqueue :: a -> StateT [a] IO ()
enqueue a = runStack (S.enqueue a)

(^) :: a -> StateT [a] IO ()
(^) a = runStack (S.push a)

v :: StateT [a] IO a
v = runStack S.pop

(^^) :: a -> StateT [a] IO ()
(^^) = enqueue

vv :: StateT [a] IO a
vv = dequeue
