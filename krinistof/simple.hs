fact a
  | a == 0 = 1
  |otherwise = a * fact (a-1) 

fib a
  | a == 0 = 0
  | a == 1 = 1
  | a == 2 = 1
  |otherwise = fib (a-1) + fib (a-2)

range :: Int -> Int -> [Int]
range a b
  | a == b = [b] 
  | a > b  =  [a] ++ range (a-1) b
  | otherwise = [a] ++ range (a+1) b

len :: [Int] -> Int
len a
  | a == [] = 0
  | otherwise = 1 + len (tail a)
everySecond :: String -> Int -> String
everySecond s i
  | i == length s = ""
  | even i = "" ++ everySecond s (i+1) 
  | otherwise = [s !! i] ++ everySecond s (i+1) 

el :: Char -> String -> Bool
el s1 s2
  | s2 == [] = False 
  | otherwise = s1 == (head s2) || el s1 (tail s2)
