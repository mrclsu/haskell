{-# OPTIONS -Wincomplete-patterns #-}
module Lofasz3 where
import Data.Bool

-- data Tree a = Leaf
--             | Bin a (Tree a) (Tree a)
--     deriving (Show)

-- height :: Tree a -> Int
-- height (Leaf) = 0
-- height (Bin _ c1 c2) = 1 + max (height c1) (height c2)

-- instance Eq a => Eq (Tree a) where
--     (==) Leaf Leaf = True 
--     (==) (Bin n1 c1 c2) (Bin n2 c21 c22) = (n1 == n2 && c1 == c21 && c2 == c22)
--     (==) _ _ = False

-- ex1.
-- BinTree amiben a Node-ban vannak az értékek, Leaf-ben nem

data BinTreeNode a = Node1 a (BinTreeNode a) (BinTreeNode a)
                    | Leaf1 

-- ex2.
-- minde levélben, mind csúcsokban, de eltérő típusú is lehet!

data BinTreeBoth l n = Leaf l
                       | Node n (BinTreeBoth l n) (BinTreeBoth l n)

concatTreeLeaves :: Semigroup l => BinTreeBoth l n -> l 
concatTreeLeaves (Leaf l) = l
concatTreeLeaves (Node _ child1 child2) = concatTreeLeaves child1 <> concatTreeLeaves child2


concatTreeNodes :: Monoid n => BinTreeBoth l n -> n 
concatTreeNodes (Leaf _) = mempty
concatTreeNodes (Node n c1 c2) = n <> concatTreeNodes c1 <> concatTreeNodes c2

concatMapTree :: Monoid m => (l -> m) -> (n -> m) -> BinTreeBoth l n -> m 
concatMapTree leafMap _ (Leaf l) = leafMap l
concatMapTree lM nM (Node n c1 c2) = nM n <> concatMapTree lM nM c1 <> concatMapTree lM nM c2