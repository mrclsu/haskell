eqBool :: Bool -> Bool -> Bool
eqBool True True = True
eqBool False False = False
eqBool _ _ = False


eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa [] [] = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList eqa _ _ = False


eqPair :: (a -> a -> Bool) -> [a] -> Bool
eqPair eqa eqb (x,y) (x', y') = eqa x x' && eqb y y'


eqBoolBool :: (Bool, Bool) -> (Bool, Bool) -> Bool
eqBoolBool = eqPair eqBool eqBool


eqListBool :: [Bool] -> [Bool] -> Bool
eqListBool = eqList eqBool


f1 :: [[[Bool]]] -> [[[Bool]]] -> Bool
f1 = eqList (eqList (eqList eqBool))

f2 :: [(Bool, [Bool])] -> [(Bool, [Bool])] -> Bool
f2 = eqList (eqPair eqBool (eqList eqBool))


class Eq' a where
    eq :: a -> a -> Bool
    
instance Eq' Bool where
    eq True True = True
    eq False False = True
    eq _ _ = False

instance Eq' a => Eq' [a] where
    eq [] [] = True
    eq (x:xs) (y:ys) = eq x y && eq xs ys
    eq _ _ = False


instance (Eq' a, Eq' b) => Eq' (a,b) where
    eq (x,y) (x', y') = eq x x' && eq y y'




data One = One
one :: One
one = One


f :: One -> Int
f One = 200

