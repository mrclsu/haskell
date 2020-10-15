import Data.Char

f :: [(Int, Int)] -> Int
f [] = 0
f [x] = 0
f ((a,b):(c,d):_) = a + b + c + d

timeAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
timeAdd (a,b) (c,d) = (mod ((a + c) + div (b + d) 60) 24, mod (b + d) 60)

isSmile :: String -> Bool
isSmile [a,b]
    | (a == ':' || a == ';') && (b == ')' || b == ']' || b == '}') = True
    | otherwise = False
isSmile _ = False 

bimBam :: Int -> String
bimBam x
    | mod x 3 == 0 && mod x 5 == 0 = "BimBam"
    | mod x 3 == 0                 = "Bim"
    | mod x 5 == 0                 = "Bam"
    | otherwise                    = ""

minList :: [Int] -> [Int] -> [Int]
minList [] _ = []
minList _ [] = []
minList (a:as) (b:bs) = (min a b) : (minList as bs)

wordNumWithCapital :: String -> Int
wordNumWithCapital l = length $ filter isUpper $ map head $ words l


oneMatrix :: Int -> [[Int]]
oneMatrix n = take n $ repeat $ take n $ repeat 1