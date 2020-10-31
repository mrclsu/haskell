module Bead05 where

import Data.Maybe
import Control.Monad

--------------------------------------------------------------------------------
newtype State s a = State { runState :: s -> (s, a) }

execState :: State s a -> s -> s
execState (State f) s = fst (f s)

evalState :: State s a -> s -> a
evalState (State f) s = snd (f s)

put :: s -> State s ()
put s = State (\_ -> (s, ()))

get :: State s s
get = State (\s -> (s, s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> (f s, ()))

instance Functor (State s) where 
  fmap f (State g) = State $ \s -> fmap f (g s)
instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad (State s) where
  return x = State (\s -> (s, x))
  State f >>= g = State (\s -> let (s', a) = f s in runState (g a) s')
--------------------------------------------------------------------------------

-- Bead assignment 05:
-- Give a "translation" of the following imperative program using the Haskell state monad:

-- a := 1
-- for i from 1 to n do
--   if a `mod` i == 0 then 
--     a := a * i

p :: Integer -> State Integer ()
p n = do
  forM_ [1..n] $ \i -> do
    a <- get 
    put(if i `mod` a == 0 then a*i else a)

runP :: Integer -> Integer
runP n = execState (p n) 1

-- Examples:
-- runP 1 == 1
-- runP 2 == 2
-- runP 3 == 2
-- runP 4 == 8
-- runP 5 == 8
-- runP 8 == 64
-- runP 9 == 64
-- runP 63 == 64
-- runP 64 == 4096
-- fmap runP [1..20] == [1,2,2,8,8,8,8,64,64,64,64,64,64,64,64,64,64,64,64,64]

