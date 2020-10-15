checks :: Int -> [Int] -> Int -> Bool
checks q b j = q == b !! j || abs ( q - b !! j) == j + 1

safe :: Int -> [Int] -> Bool
safe q b = and [ (checks q b i) | i <- [0..length b - 1]]

type Board = [Int]

queens :: Int -> [Board] 
queens 0 = [[]]
queens n = [ q:b | b <- queens (n-1), q <- [0..7], safe q b]