{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Homework3 where

  newtype F1 x a   = F1 (x -> a)
  newtype F2 x a   = F2 ((a -> x) -> a)
  newtype F3 x y a = F3 ((a -> x) -> (a -> y) -> a)
  newtype F4 x y a = F4 (((y -> a) -> x) -> a)
  newtype F5 x a   = F5 (x -> (x -> a))
  newtype F6 x y a = F6 (x -> (a -> y) -> (x -> a))
  newtype F7 x y a = F7 (x -> ((a -> x) -> a))

  instance Functor (F1 x) where
    fmap :: (a -> b) -> (F1 x a) -> (F1 x b)
    fmap f (F1 g) = F1 $ f . g

  instance Functor (F2 x) where
    fmap :: (a -> b) -> (F2 x a) -> (F2 x b)
    fmap f (F2 g) = F2 