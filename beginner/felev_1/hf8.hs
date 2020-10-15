module HF8 where
import Prelude hiding (permutations)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort $ filter (x >) xs) ++ [x] ++ (qsort $ filter (x <=) xs)

insertions:: a -> [a] -> [[a]]
insertions a x = [ (take n x) ++ [a] ++ (drop n x) | n <- [0..length x]]

permutations :: [a] -> [[a]]
permutatins [] = [[]]
permutations [a] = [[a]]
permutations (x:xs) = concat (map (insertions x) (permutations xs))