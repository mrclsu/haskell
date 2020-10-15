import qualified Data.Char as Char
import qualified Data.List as List


--4/3
nullInt :: [Int] -> Bool
nullInt t = length t == 0


--4/4
isSingleTonInt :: [Integer] -> Bool
isSingleTonInt a = length a == 1


--4/5
toUpperFirst :: String -> String
toUpperFirst (hd:tl) = Char.toUpper hd : tl
toUpperFirst [] = [] 


--4/6
isLetter :: Char -> Bool
isLetter a = elem (Char.toLower a) ['a'..'z']


--4/7
isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']


--4/8
mountain :: Integer -> [Integer]
mountain n = [(n-abs x) | x <- [(-n+1)..(n-1)]]


--4/9
divisors :: Integer -> [Integer]
divisors a = [n | n <- [1..a], mod a n == 0 ]


--4/10
powersOfTwo :: [Integer]
powersOfTwo = [2^n | n <-[0..]]


--4/11
pi' :: Float
pi' =  4 * sum [1/y | y <-[(((-1) ** x) * (2*x+1)) | x <-[0..1000]]]


--4/12
time :: [(Int, Int)]
time = [(h, m) | h <- [0..23], m <- [0..59]]

--5/2
primes ::  [Integer]
primes = [n | n <- [3..], length [x | x <- [2.. div n 2], mod n x == 0] == 0]


--5/5
alphabet :: [(Integer, Char)]
alphabet = zip [0..] ['a'..'z']


--5/7
students :: [(String, [(String, String, String)])] -> [String]
students [(p,[(_,_,n)])] = 
    if p == "Funkcionalis programozas" 
    then [n] 
    else []
students [(p,(_,_,n):t1)] = 
    if p == "Funkcionalis programozas" 
    then n : students [(p, t1)] 
    else students [(p, t1)]
students ((p,(_,_,n):t1):t2) = 
    if p == "Funkcionalis programozas" 
    then n : students [(p, t1)] ++ students t2 
    else students [(p, t1)] ++ students t2


--5/9
comp :: [String] -> [(Int, Char)]
comp [s] = [(length s, head s)]
comp (s:tl) = (length s, head s) : comp tl

compress :: String -> [(Int, Char)]
compress st = comp (List.group st)


--5/10
decompress :: [(Int, Char)] -> String
decompress [(i, c)] = replicate i c
decompress ((i, c):tl) = replicate i c ++ decompress tl