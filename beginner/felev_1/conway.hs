type Coordinate = (Integer, Integer)

type Generation = [Coordinate]

single :: Generation
single = [ (42, 42) ]

block :: Generation
block = [ (5, 6), (5, 7)
        , (6, 6), (6, 7) ]

row :: Generation
row = [ (10, 1), (10, 2), (10, 3) ]

column :: Generation
column = [ (9,  2)
        , (10, 2)
        , (11, 2) ]

caterer :: Generation
caterer = [ (2, 4), (3, 2), (3, 6), (3, 7), (3, 8), (3, 9)
    , (4, 2), (4, 6), (5, 2), (6, 5), (7, 3), (7, 4) ]



neighbors :: Coordinate -> [Coordinate]
neighbors (x,y) = [ (a,b) | a <- [x-1,x,x+1] , b <- [y-1,y,y+1], not ((a==x) && (b==y))]


livingNeighbors :: Generation -> Coordinate -> Int
livingNeighbors [] _ = 0
livingNeighbors (h@(a,b):hs) c@(x,y)
    | elem h (neighbors c) = 1 + livingNeighbors hs c
    | otherwise = livingNeighbors hs c


staysAlive :: Generation -> Coordinate -> Bool
staysAlive [] _ = False
staysAlive l c
    | nCells == 4 = False
    | nCells < 2  = False
    | otherwise   = True
    where
        nCells = length [ x | x <- l, elem x (neighbors c)] 


stepLivingCells :: Generation -> Generation
stepLivingCells g = filter (staysAlive g) g


deadNeighbors :: Generation -> [Coordinate]
deadNeighbors g = nub [ x | c <- g, x <- (neighbors c), not (elem x g)]


stepDeadCells :: Generation -> Generation
stepDeadCells g = nub [ x | x <- (deadNeighbors g), not (elem x g) && (staysAlive g x)]

stepCells :: Generation -> Generation
stepCells g = sort (nub ((stepDeadCells g) ++ (stepLivingCells g)))
    where
        sort [] = []
        sort [x] = [x]
        
        