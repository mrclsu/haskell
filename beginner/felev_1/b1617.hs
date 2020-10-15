module B16717 where

isAmenable :: Int -> Bool
isAmenable 4 = False
isAmenable n = m == 0 || m == 1
 where
  m = mod n 4

upgrade :: (String, String, Int) -> (String, String, Int, Bool)
upgrade (id, brand, year) = (id, brand, year, ok)
 where
  ok = elem brand ["Ford", "Suzuki"] && year < 2000
--ok = (brand == "Ford" || brand == "Suzuki") && year < 2000

expandPath :: String -> String -> String
expandPath home ('~':path@('/':_)) = home ++ path
expandPath _ path                  = path

binom :: Integer -> Integer -> Integer
binom n k = (fact n) `div` (fact (n-k) * fact k)
 where
  fact n = product [1..n]

quasiMagicSquare :: [[Int]] -> Bool
quasiMagicSquare ls = allEq sums
 where
  sums = [ sum x | x <- ls ]
  allEq [x] = True
  allEq (x1:a@(x2:xs)) = x1 == x2 && allEq a

simpleGrep :: String -> String -> [(String, Int)]
simpleGrep word file = [ a | a@(str, i) <- zip lns [1..], take len str == word ]
 where
  lns = lines file
  len = length word
  
hasNotTranslator :: [(String, String)] -> [(String, String, String)] -> [String]
hasNotTranslator countries interpreters = [ country | (country, language) <- countries, notElem language okLanguages ]
 where
  okLanguages = [ language | (_, _, language) <- interpreters ]
