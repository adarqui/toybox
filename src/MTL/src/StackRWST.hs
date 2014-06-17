module StackRWST (
 pop,
 push,
 contents
 {-
 (^),
 v,
 enqueue,
 dequeue,
 (^^),
 vv
 -}
) where


import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Prelude hiding ((^), (^^))

import qualified Stack as S

runStack p = do
 st <- get
 let (a,s) = runState p st
 put s
 return a

contents :: (Monoid w) => RWST r w [a] IO [a]
contents = do
 st <- get
 return st

pop :: (Monoid w) => RWST r w [a] IO a
pop = runStack S.pop

dequeue :: (Monoid w) => RWST r w [a] IO a
dequeue = runStack S.dequeue

push :: (Monoid w) => a -> RWST r w [a] IO ()
push a = runStack (S.push a)

enqueue :: (Monoid w) => a -> RWST r w [a] IO ()
enqueue a = runStack (S.enqueue a)

(^) :: (Monoid w) => a -> RWST r w [a] IO ()
(^) a = runStack (S.push a)

v :: (Monoid w) => RWST r w [a] IO a
v = runStack S.pop

(^^) :: (Monoid w) => a -> RWST r w [a] IO ()
(^^) = enqueue

vv :: (Monoid w) => RWST r w [a] IO a
vv = dequeue
