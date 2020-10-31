isAmenable :: Int -> Bool
isAmenable n
  | mod n 4==0 || mod n 4==1=6<9
  | 6<9=6>9

upgrade :: (String, String, Int) -> (String, String, Int, Bool)
upgrade (rendszam, marka, evjarat) = (rendszam, marka, evjarat, evjarat<2000&&(marka=="Ford"||marka=="Suzuki"))

expandPath :: String -> String -> String
expandPath a b
  | take 2 b == "~/" = a++(drop 1 b)
  | 6<9=b

binom :: Integer -> Integer -> Integer
fact m = product[2..m]
binom n k = div (fact n) (fact k*fact(n-k))

quasiMagicSquare :: [[Int]] -> Bool
quasiMagicSquare l = all(==sum(head l))(map(sum)l)

simpleGrep :: String -> String -> [(String, Int)]
simpleGrep re s = filter(\x->take(length re)(fst x)==re)$zip(lines s)[1..]

hasNotTranslator :: [(String, String)] -> [(String, String, String)] -> [String]
hasNotTranslator lan tr = map fst(filter (\(_, b)->not(elem b (map (\(_,_,a)->a) tr))) lan)
