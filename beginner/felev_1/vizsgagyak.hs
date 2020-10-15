import Data.Char
import Data.List

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a,b) = ((min a b), (max a b))

caseSwap :: Char -> Char
caseSwap c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c


count :: Eq a => a -> [a] -> Int
count x ls = length $ filter (==x) ls

listMul :: [Int] -> [Int] -> Int
listMul a b = sum [ (a !! i) * (b !! i) | i <- [0.. (min (length a) (length b))-1]]

sameSign :: [Int] -> Bool
sameSign ls = (length (nub (map signum (filter (/=0) ls)))) < 2

isCorrect :: [(Int, Int)] -> Bool
isCorrect [] = True
isCorrect [_] = True
isCorrect ((_,a):x@(b,_):xs) = a == b && isCorrect (x:xs)


filterMany :: [a -> Bool] -> [a] -> [a]
filterMany [] ls = ls
filterMany (f:fs) ls = filterMany fs $ filter f ls


conditionalMax :: Ord a => (a -> Bool) -> [a] -> Maybe a
conditionalMax f ls
    | length fs > 0 = Just $ maximum fs
    | otherwise = Nothing
    where
        fs = filter f ls


data Season = Winter | Spring | Summer | Autumn
        deriving(Eq, Show)

nextSeason :: Season ->  Season
nextSeason Winter = Spring
nextSeason Spring = Summer
nextSeason Summer = Autumn
nextSeason Autumn = Winter


seasonAfterMonths :: Int -> Season
seasonAfterMonths x
    | y < 2 = Winter
    | y < 5 = Spring
    | y < 8 = Summer
    | y < 11 = Autumn
    | otherwise = Winter
    where
        y = mod x 12


removeSpecial :: String -> String
removeSpecial s = filter (\c -> elem c xx) s
    where
        xx = ['a'..'z'] ++ ['A'..'Z'] ++ [' '] ++ ['0'..'9']


isSublist :: Eq a => [a] -> [a] -> Bool
isSublist a [] = False
isSublist a b@(_:bs)
    | [ x | (x, y) <- zip a b, x == y ] == a = True
    | otherwise = isSublist a bs


multipleElems :: Eq a => [a] -> [a]
multipleElems [] = []
multipleElems (x:xs)
    | elem x xs = x : (multipleElems $ filter (/=x) xs)
    | otherwise =  multipleElems xs


maxTempChange :: [(Int, Int)] -> Int
maxTempChange ls = snd $ maximumBy (\(a,_) (b,_) -> compare a b) (zip (map (\(a,b) -> b-a) ls) [1..])


primeIndex :: [a] -> [a]
primeIndex ls = map (\(a,_) -> a) (filter (\(_,i) -> length [x | x <- [1..i], mod i x == 0] == 2) (zip ls [1..]))