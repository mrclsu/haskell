import Data.List
import Data.Char

doSomething :: String -> String
doSomething [] = []
doSomething [x] = [x]
doSomething s 
    | length x == 1 = concat ([x] ++ [(doSomething xs)])
    | otherwise = (intToDigit (length x)) : ([head x] ++ (doSomething xs))
    where
        x = take (length (head $ group s)) s
        xs = drop (length x) s


undoSomething :: String -> String
undoSomething [] = []
undoSomething [x] = [x]
undoSomething (a:b:xs)
    | isDigit a = concat ([(replicate ((digitToInt a)-1) b)] ++ [(undoSomething (b:xs))])
    | otherwise = a : undoSomething (b:xs)

