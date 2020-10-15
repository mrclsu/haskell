digits :: Integer -> [Integer]
digits 0 = []
digits n = (mod n 10) : digits (div n 10)

squareSum :: [Integer] -> Integer
squareSum [] = 0
squareSum (x:xs) = x^2 + squareSum xs

happy' :: Integer -> Integer -> Bool
happy' 1 _ = True
happy' _ 1000 = False
happy' x n = happy' (squareSum  (digits x)) (n+1)

happy :: Integer -> Bool
happy x = happy' x 1

happyNumbers :: [Integer]
happyNumbers = [x | x <- [1..500], happy x]