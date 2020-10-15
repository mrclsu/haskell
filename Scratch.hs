module Scratch where

  fmap' :: (Num a, Functor f) => f a -> f a
  fmap' func = fmap (*2) func

