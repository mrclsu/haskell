import Data.List
--Algebrai adattípusok
{-
data Shape = Circle | Rectangle
--    deriving (Show)

instance Show Shape where
    show Circle = "C"
    show Rectangle = "R"


instance Eq Shape where
    Circle == Circle = True
    Rectangle == Rectangle = True
    _ == _ = False
-}

data Shape = Circle Double | Rectangle Double Double
    deriving (Eq, Show)

kerület :: Shape -> Double
kerület (Circle r) = 2 * r * pi
kerület (Rectangle a b) = 2 * a + 2 * b

terület :: Shape -> Double
terület (Circle r) = r * r * pi
terület (Rectangle a b) = a * b



data Gender = Male | Female | Other
    deriving (Eq, Show)

--data Person = P String Int Gender
--    deriving (Eq, Show)
--
--getName :: Person => String
--getName (P name _ _) = name

data Person = 
    P {
        name    :: String,
        age     :: Int,
        gender  :: Gender
    }
    deriving (Eq, Show)

ihat :: Person -> Bool
ihat p = age p > 17

--legidosebb :: [Person] -> Person
--legidosebb [x] = x
--legidosebb (x:xs) = if age x > (age $ legidosebb xs) then x else legidosebb xs


legidosebb :: [Person] -> Person
legidosebb = maximumBy (\(P _ age1 _) (P _ age2 _) -> compare age1 age2)


circles :: [Shape] -> [Shape]
circles ls = [ x | x@(Circle _) <- ls]
--circles [] = []
--circles (x@(Circle _):xs) = x : circles xs
--circles (_:xs) = circles xs