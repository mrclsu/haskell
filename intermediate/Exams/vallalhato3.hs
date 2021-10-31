{-# OPTIONS -Wincomplete-patterns #-}
module Lofasz where

  import Data.Bool;

  data List a = Nil 
    | Cons a (List a)

  lengthL :: List a -> Int
  lengthL Nil = 0
  lengthL (Cons _ xs) = 1 + lengthL xs

  eqL :: Eq a => List a -> List a -> Bool
  eqL Nil Nil = True
  eqL (Cons x xs) (Cons y ys) = x == y && eqL xs ys
  eqL _ _ = False


  ex1List :: List Int
  ex1List = Cons 1 $ Cons 2 $ Cons 3 $ Nil

  ex2List :: List Int
  ex2List = Cons 1 $ Cons 6 $ Cons 3 $ Nil

  ex3List :: List Int
  ex3List = Nil

  ex4List :: List Int
  ex4List = Cons 1 $ Cons 6 $  Cons 6 $  Cons 6 $  Cons 6 $  Cons 6 $ Cons 3 $ Nil


  length' :: [a] -> Int
  length' [] = 0
  length' (x:xs) = 1 + length xs


  error' :: [Char] -> a
  error' x = error' x

  data Nat = Zero
          | Suc Nat
  zero :: Nat
  zero = Zero

  three :: Nat
  three = Suc $ Suc $ Suc Zero

  six :: Nat
  six = Suc $ Suc $ Suc $ Suc $ Suc $ Suc Zero

  toZero :: Nat -> Int
  toZero _ = 0

  toInt :: Nat -> Int
  toInt Zero = 0
  toInt (Suc rest) = (+) 1 $ toInt rest

  eqNat :: Nat -> Nat -> Bool
  eqNat Zero Zero = True
  eqNat (Suc r1) (Suc r2) = (&&) True $ eqNat r1 r2
  eqNat _ _ = False

  addNat :: Nat -> Nat -> Nat
  addNat Zero Zero = Zero
  addNat (Suc r1) (Suc r2) = Suc $ Suc $ addNat r1 r2
  addNat (Suc r1) Zero = Suc $ r1
  addNat Zero (Suc r2) = Suc $ r2

  mulNat :: Nat -> Nat -> Nat
  mulNat (Suc r) o = addNat o $ mulNat r o
  mulNat _ Zero = Zero
  mulNat Zero _ = Zero

  lengthL' :: List a -> Nat
  lengthL' Nil = Zero
  lengthL' (Cons _ xs) = Suc $ lengthL' xs