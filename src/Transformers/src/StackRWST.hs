module StackRWST (
 pop,
 push,
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
{-
import Control.Monad.Reader
import Control.Monad.Writer
-}
import qualified Control.Monad.State as ST
import Control.Monad.Trans.RWS
import Prelude hiding ((^), (^^))

import Data.Monoid

import qualified Stack as S

runStack p = do
 st <- get
 let (a,s) = ST.runState p st
 put s
 return a

pop :: (Data.Monoid.Monoid w) => RWST r w [a] IO a
pop = runStack S.pop

{-
dequeue :: RWST r w [a] IO a
dequeue = runStack S.dequeue
-}

push :: (Data.Monoid.Monoid w) => a -> RWST r w [a] IO ()
push a = runStack (S.push a)

{-

enqueue :: a -> RWST r w [a] IO ()
enqueue a = runStack (S.enqueue a)

(^) :: a -> RWST r w [a] IO ()
(^) a = runStack (S.push a)

v :: RWST r w [a] IO a
v = runStack S.pop

(^^) :: a -> RWST r w [a] IO ()
(^^) = enqueue

vv :: RWST r w [a] IO a
vv = dequeue
-}
