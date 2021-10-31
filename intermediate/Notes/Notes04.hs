{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

import Control.Monad (ap)
import Control.Monad.State ( execState, State, put, get, modify )

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Show, Eq, Ord)

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5

expr2 :: IntExpr
expr2 = Value 10 `Times` expr1

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 5

expr4 :: IntExpr
expr4 = Value 10 `Div` Value 0

-- Define `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 150
--   evalIntExpr expr3 == 2
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr (Value a) = a
evalIntExpr (Plus a b) = (evalIntExpr a) + (evalIntExpr b)
evalIntExpr (Times a b) = (evalIntExpr a) * (evalIntExpr b)
evalIntExpr (Div a b) = div (evalIntExpr a) (evalIntExpr b)

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 150
--   evalIntExprMaybe expr3 == Just 2
--   evalIntExprMaybe expr4 == Nothing
-- Hint: first define
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe (Value a) = Just a
evalIntExprMaybe (Plus a b) = (evalIntExprMaybe a) >>= \ma ->
                            (evalIntExprMaybe b) >>= \mb -> 
                                Just (ma+mb)
evalIntExprMaybe (Times a b) = (evalIntExprMaybe a) >>= \ma ->
                            (evalIntExprMaybe b) >>= \mb -> 
                                Just (ma*mb)
evalIntExprMaybe (Div a b) = (evalIntExprMaybe a) >>= \ma ->
                            (evalIntExprMaybe b) >>= \mb -> 
                                safeDiv ma mb


-- Some operations on monads
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do 
            a <- ma
            return $ f a


liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
            a <- ma
            b <- mb
            return $ f a b

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = do
            a <- ma
            b <- mb
            c <- mc
            return $ f a b c

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f [] _ = return []
zipWithM f _ [] = return []
zipWithM f (a:as) (b:bs) = liftM2 (:) (f a b) (zipWithM f as bs)
--zipWithM f (a:as) (b:bs) = do
--    c <- (f a b)
--    cs <- zipWithM f as bs
--    return (c:cs)


foldrM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldrM' f e [] = return e
foldrM' f e (x:xs) = do 
    y <- foldrM' f e xs
    f y x
-- Only look after this line after having defined everything above.

-- Define using the State monad:

-- impFactorial should be a translation of the imperative program
--    x = 1
--    for i from 1 to n
--      x = x * i

impFactorial :: Integer -> State Integer ()
impFactorial n = undefined

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    a = 1
--    b = 1
--    for i from 1 to n
--      (a, b) = (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = undefined

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))

-- x | o | x
-------------
-- o | o | x  
-------------
-- x | x | o

