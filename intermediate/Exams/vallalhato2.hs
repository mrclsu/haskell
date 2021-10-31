{-# OPTIONS -Wincomplete-patterns #-}
module Lofasz2 where

    data Color = Red | Green | Blue

    instance Eq Color where
        (==) Red Red = True
        (==) Green Green = True
        (==) Blue Blue = True
        (==) _ _ = False

    data List a = Nil
                | Cons a (List a)
                deriving (Show)

    instance Eq a => Eq (List a) where
        Nil       == Nil       = True
        Cons x xs == Cons y ys = (x == y) && (xs == ys)
        _         == _         = False

    data Either' a b = Left'  a
                    |  Right' b
                    deriving (Show)

    instance (Eq a, Eq b) => Eq (Either' a b) where
        Right' a == Right' aa = (a == aa)
        Left' b == Left' bb = (b == bb)
        _ == _ = False

    data Prod a b = Pair a b
                deriving (Show)
    instance (Eq a, Eq b) => Eq (Prod a b) where
        (==) (Pair a b) (Pair aa bb) = a == aa && b == bb

    data BinTree a = Leaf a
                   | Node (BinTree a) (BinTree a)
                   deriving (Show)
    
    instance (Eq a) => Eq (BinTree a) where
        (==) (Leaf a) (Leaf b) = a == b
        (==)  (Node a b) (Node aa bb) = a == aa && b == bb
        (==) _ _ = False