module First where

divides :: Int -> Int -> Bool
divides x y = mod x y == 0

divisors :: Int -> [Int] 
divisors n = [ i | i <- [1..n], divides n i]

isPrime :: Int -> Bool 
isPrime n = length (divisors n) == 2


