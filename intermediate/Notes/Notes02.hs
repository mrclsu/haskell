module Notes02 where

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just $ f x

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (a, c) = (f a, g c)

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f g (Left a)  = Left $ f a
mapEither f g (Right c) = Right $ g c

-- Define f and g using mapList, mapMaybe, mapPair, mapEither, ...
f :: (a -> b) -> [[a]] -> [[b]]
--f ff (x:xs) = (mapList ff x) : f ff xs
f ff = mapList $ mapList ff

g :: (a -> b) -> [(a, a)] -> [(a, b)]
g ff = mapList (mapPair id ff)

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)
             deriving (Eq, Ord, Show)

mapTree1 :: (a -> b) -> Tree1 a -> Tree1 b
mapTree1 ff (Leaf1 a)   = Leaf1 $ ff a
mapTree1 ff (Node1 a b) = Node1 (mapTree1 ff a) (mapTree1 ff b)


data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 ff (Leaf2 a) = Leaf2 $ ff a
mapTree2 ff (Node2 a) = Node2 (mapList (mapTree2 ff) a)

-- Only look below this line after you have defined all the functions above

mapFun :: (a -> b) -> (Int -> a) -> (Int -> b)
--mapFun f g = f . g
mapFun = (.)

data Tree3 a = Leaf3 a
             | Node3 (Int -> [Tree3 a]) 
mapTree3 :: (a -> b) -> Tree3 a -> Tree3 b
mapTree3 f (Leaf3 a) = Leaf3 $ f a
mapTree3 f (Node3 a) = Node3 (\x -> ( map (mapTree3 f) (a x)))


bindList :: (a -> [b]) -> [a] -> [b]
bindList f l = l >>= f
-- example: bindList (\x -> [x, x+1]) [1, 2] == [1, 2, 2, 3]

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f Nothing  = Nothing
bindMaybe f (Just a) = f a
-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 f (Leaf1 a)  = f a
bindTree1 f (Node1 a b) = Node1 (bindTree1 f a) (bindTree1 f b)
-- example:
--  bindTree1 (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
--            (Node1 (Leaf1 True) (Leaf1 False)))
--  == Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))
ex1 = bindTree1 
        (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
        (Node1 (Leaf1 True) (Leaf1 False))
ex2 = Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))


data Fraction a = Integral a a

addFrac :: Fraction a -> Fraction a -> Fraction a
addFrac (a b) (c d) = undefined
              
        