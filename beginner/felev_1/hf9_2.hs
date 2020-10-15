import Data.List

censorship :: (String, String) -> String -> String
censorship (_,_) [x] = [x]
censorship (a,b) s@(x:xs)
    | isPrefixOf a s = (head b) : censorship (a,b) (tail (b ++ (drop (length a) s)))
    | otherwise     = x : censorship (a,b) xs


primeFactorization :: Int -> [(Int, Int)]
primeFactorization n = zip (primeDividers n) (map (power n) $ primeDividers n)
    where
        primeDividers n = [x | x <- [2..n],
            mod n x == 0, null [ y | y <- [2.. (div x 2)], mod x y == 0]]
        power n x = if mod n x == 0 then power (div n x) x + 1 else 0