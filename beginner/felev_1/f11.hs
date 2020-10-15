import Prelude hiding (group)

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x:a):group b
    where
        (a,b) = span (==x) xs



compress :: Eq a => [a] -> [(Int,a)]
compress [] = []
compress l@(x:xs) = (length a, x) : compress b
    where
        (a,b) = span (==x) l

--pt :: [Int] -> [Int]


pascalTriangle :: [[Integer]]
pascalTriangle = p [1]
    where
        p ls = 0:x : (pt 0:x)
        x = pt ls
        pt [] = []
        pt [x] = [x]
        pt (a:b:xs) = (a+b) : pt (b:xs)

        