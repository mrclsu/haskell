{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Homework3 where
import Prelude hiding (concat)

newtype DList a = DL { unDL :: [a] -> [a] }

fromList :: [a] -> DList a
fromList = DL . (++)

toList :: DList a -> [a]
toList = ($[]) . unDL

apply :: DList a -> [a] -> [a]
apply = (++) . toList

empty :: DList a
empty = DL id

singleton :: a -> DList a
singleton a = fromList([a])

cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

concat :: [DList a] -> DList a
concat = foldr append empty

instance Functor DList where 
  fmap :: (a -> b) -> (DList a) -> (DList b)
  fmap f (DL g) = DL $ g . (map f)

-- instance Functor (AdvanceDList a) where 
--   fmap :: (n -> m) -> (AdvanceDList a n) -> (AdvanceDList a m)
--   fmap f (ADL g) = ADL $ fmap f . g  
--   -- fmap :: (a -> b) -> (AdvanceDList a) -> (AdvanceDList b)
--   -- fmap = unADL
--   -- fmap = undefined
--   -- fmap _ empty = empty
--   -- fmap f (x:xs) = (f x) : (fmap f xs)