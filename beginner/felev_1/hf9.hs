import Data.List

censorship :: (String, String) -> String -> String
censorship (a, b) t = unwords [ if isInfixOf a w then b ++ (drop (length a) w) else w | w <- words t]


primeFactorization :: Int -> [(Int, Int)]
primeFactorization n = map (\x -> (x, power n x)) [ x | x <- [2..n], mod n x == 0, null [ y | y <- [2..(div x 2)], mod x y == 0]]
    where
        power n p = if mod n p == 0 then power (div n p) p + 1 else 0

