primes :: [Integer]
primes = [ n | n <- [2..], length [i | i <- [2.. div n 2], (mod n i) == 0 ] == 0]

eulersNumber :: Double -> Double
eulersNumber n = sum [ (1 / (product [1..i])) | i <- [1..n]] + 1