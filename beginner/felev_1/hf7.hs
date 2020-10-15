desc :: Int -> [(Int, Int)]
desc n = [(a,b) | a <- [0..n-1], b <- [a+1..n]] 


anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) = if x then x else anyTrue xs


removeItem :: [Int] -> Int -> [Int]
removeItem [] _ = []
removeItem (x:xs) 0 = removeItem xs (-1)
removeItem (x:xs) n = x : removeItem xs (n-1)


removeItems :: [Int] -> Int -> Int -> [Int]
removeItems [] _ _ = []
removeItems (x:xs) a b 
    | a == 0 || b == 0 = removeItems xs (a-1) (b-1) 
    | otherwise        = x : removeItems xs (a-1) (b-1)


step :: Int -> Int -> [Int] -> Bool
step a b (x) = possible ((removeItems x a b) ++ [abs((x !! a) - (x !! b))])


possible :: [Int] -> Bool
possible [0] = True 
possible x = anyTrue [ step a b x | (a,b)<- desc ((length x)-1)]