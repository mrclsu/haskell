{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Homework1 where

  data Nat = Zero | Suc Nat
    deriving Show

  addNat :: Nat -> Nat -> Nat
  addNat Zero Zero = Zero
  addNat (Suc r1) (Suc r2) = Suc $ Suc $ addNat r1 r2
  addNat (Suc r1) Zero = Suc $ r1
  addNat Zero (Suc r2) = Suc $ r2

  mulNat :: Nat -> Nat -> Nat
  mulNat (Suc r) o = addNat o $ mulNat r o
  mulNat _ Zero = Zero
  mulNat Zero _ = Zero

  eqNat :: Nat -> Nat -> Bool
  eqNat Zero Zero = True
  eqNat (Suc r1) (Suc r2) = (&&) True $ eqNat r1 r2
  eqNat _ _ = False

  data List a = Nil | Cons a (List a)
    deriving (Show, Eq, Ord)

  product' :: List Nat -> Nat
  product' Nil = Suc (Zero)
  product' (Cons a as) = mulNat a $ product' as

  mapList :: (a -> b) -> (List a) -> (List b)
  mapList _ Nil = Nil
  mapList f (Cons a as) = Cons (f a) (mapList f as)

  (+++) :: List a -> List a -> List a
  (+++) Nil Nil = Nil
  (+++) (Cons a as) (Cons b bs) = Cons a $ (+++) as (Cons b bs)
  (+++) (Cons a as) Nil = Cons a as
  (+++) Nil (Cons b bs) = Cons b bs

  eqList :: Eq a => List a -> List a -> Bool
  eqList (Cons a as) (Cons b bs) = a == b && eqList as bs
  eqList Nil Nil = True
  eqList _ _ = False

  data Tree a b = Leaf a | Bin b (Tree a b) (Tree a b)
    deriving Show

  mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
  mapTree f _ (Leaf a) = (Leaf (f a))
  mapTree f g (Bin b rem1 rem2) = (Bin (g b) (mapTree f g rem1) (mapTree f g rem2))

  eqTree :: Show a => Eq a => Show b => Eq b => Tree a b -> Tree a b -> Bool
  eqTree (Leaf n) (Leaf m) = n == m
  eqTree (Bin n rem1 rem2) (Bin m rem12 rem22) = n == m && (eqTree rem1 rem12) && (eqTree rem2 rem22)
  eqTree _ _ = False
