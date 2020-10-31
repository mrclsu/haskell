module Bead02 where

-- Bead assignment 02:
--   Define an `Functor` instance for `T`.

data T a = A Int (Maybe a)
         | B (T a)
         deriving (Show, Eq)

instance Functor T where
  -- fmap :: (a -> b) -> T a -> T b
  fmap f (A a (Just b)) = (A a (Just (f b)))
  fmap f (A a Nothing)  = (A a Nothing)
  fmap f (B a)          = B (fmap f a)

-----------
-- Tests --
-----------

{-
fmap (+ 1) (A 0 (Just 1))        == A 0 (Just 2)
fmap (+ 1) (A 1 Nothing)         == A 1 Nothing
fmap id    (B (B (A 2 Nothing))) == B (B (A 2 Nothing))
fmap (* 2) (B (A 3 (Just 10)))   == B (B (A 0 (Just 20)))
-}
