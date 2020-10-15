import Data.List
{-
Legoptimálisabb (legolcsóbb) útvonal megkeresése két pont között egy súlyozott gráfban

A program egy súlyozott gráfból (Graph típus) létrehozza a szomszédsági mátrixot, amiből kiszámolható az út.

Egy 10 pontos gráfot betápláltam példának a costs függvénybe, de bármekkorával működik.
A test függvény megkeresi az utat a 0 ponttól az 5-ig (három lehetséges útvonal közül)

A best függvény változtatásával másfajta útvonalakat is lehet keresni (pl: legrövidebb, legdrágább, leghosszabb)

Példa gráf rajza: https://people.inf.elte.hu/jxtbv0/graf.png

-}

type Matrix = [[Int]]
type Path = [(Int, Int)]
type Graph = [(Int, Int, Int)]

mat :: Int -> Matrix
mat n = [[ 0 | x <- [1..n] ] | y <- [1..n] ]

costs :: Graph
costs = [(0,1,4), (9,2,8), (2,7,4), (7,5,8), (2,4,4), (4,5,2), (8,5,7),
     (8,6,3), (6,3,6), (1,7,9), (1,3,1), (9,1,2)]


insertion :: Int -> Int -> Int -> Matrix -> Matrix
insertion x y a m = (take y m) ++ [insertRow a x (m !! y)] ++ (drop (y+1) m)
     where 
          insertRow x n r = (take n r) ++ [x] ++ (drop (n+1) r)


neighborMatrix :: Graph -> Matrix -> Matrix
neighborMatrix [] m = m
neighborMatrix ((x,y,a):ls) m = neighborMatrix ls (insertion x y a (insertion y x a m))


showMatrix :: Matrix -> IO()
showMatrix m = putStr $ concat [ (show r) ++ ['\n'] | r <- m ]


totalCost :: Path -> Int
totalCost p = sum (map (\(_, c) -> c) p)


containsNode :: Int -> Path -> Bool
containsNode x p = any (==x) $ map (\(n, _) -> n) p

best :: [Path] -> Path
best [] = []
best ls = minimumBy pComp ls
     where
          pComp a b = compare (totalCost a) (totalCost b)


optimal :: Int -> Path -> Matrix -> Path
optimal dest path m = if containsNode dest path || next == [] then path else next
     where 
          next = best [ cont dest path x y m | x <- [0..(length m)-1], (c x (y path)) /= 0, not $ containsNode x path, containsNode dest (cont dest path x y m)]
          cont dest path x y m = optimal dest (path ++ [(x, (c x (y path)))]) m
          step x y = (x, c)
          c x y = ((m !! x) !! y)
          y path = fst (last path)


getOptimalPath :: Int -> Int -> Graph -> Path
getOptimalPath from to gr = optimal to start (neighborMatrix gr $ mat ((maximum (map (\(x,_,_) -> x) gr)) + 1))
     where start = [(from, 0)]

printPath :: Path -> String
printPath [] = ""
printPath [(x,_)] = show x
printPath ((x,_):xs) = (show x) ++ " -> " ++ printPath xs


test = printPath path ++ " ; osszkoltseg: " ++ show (totalCost path)
     where
          path = getOptimalPath 0 5 costs