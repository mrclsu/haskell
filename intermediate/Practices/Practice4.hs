{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Practice4 where
import Data.Monoid
import Data.Foldable

-- data Sum a b = L a | R b
--   deriving (Eq, Show, Ord)

-- instance Functor (Sum a) where 
--   fmap :: (b -> c) -> Sum a b -> Sum a c  
--   fmap _ (L a) = L a
--   fmap f (R b) = R $ f b

-- data Prod a b = P a b 
--   deriving (Eq, Ord, Show)

-- instance Functor (Prod a) where 
--   fmap :: (b -> c) -> Prod a b -> Prod a c
--   fmap f (P a b) = P a $ f b

-- data Id a = Id a 
--   deriving (Eq, Ord, Show)

-- instance Functor Id where
--   fmap :: (a -> b) -> Id a -> Id b
--   fmap f (Id a) = Id $ f a

-- data Const a b = Const a 
--   deriving (Eq, Ord, Show)

-- instance Functor (Const a) where
--   fmap :: (b -> c) -> Const a b -> Const a c
--   fmap _ (Const a) = Const a

-- data List a = Nil             -- []
--             | Cons a (List a) -- (:)
--   deriving (Eq, Ord, Show)

-- instance Functor List where 
--   fmap :: (a -> b) -> List a -> List b
--   fmap _ Nil = Nil
--   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- data BinTree l n = Leaf l | Node n (BinTree l n) (BinTree l n)
--   deriving (Eq, Show, Ord)

-- instance Functor (BinTree l) where 
--   fmap :: (n -> m) -> BinTree l n -> BinTree l m
--   fmap _ (Leaf l) = Leaf l
--   fmap f (Node n leftSub rightSub) = Node (f n) (fmap f leftSub) (fmap f rightSub) 

-- newtype BinTreeFlipped n l = Flip (BinTree l n) 
--   deriving (Eq, Ord, Show)

-- -- instance Functor (BinTreeFlipped n) where 
-- --   fmap :: (l -> k) -> BinTreeFlipped n l -> BinTreeFlipped n k
-- --   fmap _ (Flip (Leaf l)) = (Flip (Leaf l))
-- --   fmap f (Flip (Node n leftSub rightSub)) = Node (f n) (fmap f leftSub) (fmap f rightSub) 

-- class Bifunctor (f :: * -> * -> *) where 
--   bimap :: (a -> b) -> (c -> d) -> f a c -> f b d 

-- instance Bifunctor BinTree where 
--   bimap :: (l -> l') -> (n -> n') -> BinTree l n -> BinTree l' n'
--   bimap = undefined

-- newtype Fun a b = Fun {getFun :: a -> b}

-- instance Functor (Fun x) where 
--   fmap :: (r -> r') -> (Fun x r) -> (Fun x r')
--   fmap = undefined


-- Test 4
newtype Swap a = Swap { getSwap :: a } deriving (Eq, Show)

instance Semigroup m => Semigroup (Swap m) where 
  (<>) (Swap a) (Swap b) = Swap (b <> a)

instance Monoid m => Monoid (Swap m) where 
  mempty = Swap (mempty)
  mappend a b = a <> b
  mconcat [] = mempty
  mconcat (x:xs) = x <> mconcat xs