module A1617 where

vector_length :: (Double, Double, Double) -> Double
vector_length (a, b, c) = sqrt (a*a + b*b + c*c)

headToBack :: [Int] -> [Int]
headToBack [] = []
headToBack [a] = [a]
headToBack (x:xs) = init xs ++ [x]

divModEq :: Int -> Int -> Bool
divModEq a b = div a b == mod a b

quadrant :: (Int, Int) -> Int
quadrant (0, _) = 0
quadrant (_, 0) = 0
quadrant (x, y)
 | x > 0 && y > 0 = 1
 | x > 0 = 4
 | y > 0 = 2
 | otherwise = 3

pair_sums :: [Int] -> [Int]
pair_sums [] = []
pair_sums [_] = []
pair_sums (x1:x2:xs) = x1 + x2 : pair_sums xs

deliveryCost :: [(String, Double, Int)] -> Int
deliveryCost [] = 0
deliveryCost ls
 | maximum weight >= 50 = 0
 | sum price >= 30000   = 5000
 | otherwise            = 10000
  where
   (name, weight, price) = unzip3 ls
 --weight = [w | (_, w, _) <- ls]
 --price  = [p | (_, _, p) <- ls]

insert ::  Int -> Int -> [(Int, Int)] -> [(Int, Int)]
insert k p [] = [(k, p)]
insert k p a@(t@(key, priority):xs)
 | p > priority || p == priority && k > key = (k, p):a
 | otherwise                                = t : insert k p xs
