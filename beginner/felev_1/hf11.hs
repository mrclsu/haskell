import Data.List

data USTime = AM Int Int | PM Int Int
    deriving (Eq)

hour :: USTime -> Int
hour (AM h _) = h
hour (PM h _) = h

data Time = Int Int
    deriving (Eq)


-- Példányosítsuk a Show típusosztályt
-- Egy időpont megjelenítésánek formátuma:
--   HH:MM a.m. vagy HH:MM p.m.
instance Show USTime where
    show (AM h m) = concat [(show $ div h 10), (show $ mod h 10), ":", (show $ div m 10), (show $ mod m 10), " a.m."]
    show (PM h m) = concat [(show $ div h 10), (show $ mod h 10), ":", (show $ div m 10), (show $ mod m 10), " p.m."]
    
-- Példányosítsuk az Ord típusosztályt
instance Ord USTime where
    compare (AM _ _) (PM _ _) = LT
    compare (PM _ _) (AM _ _) = GT
    compare (AM h1 m1) (AM h2 m2) | i1 > i2 = GT | i1 < i2 = LT | otherwise = EQ 
        where
            i1 = h1*60+m1
            i2 = h2*60+m2
    compare (PM h1 m1) (PM h2 m2) | i1 > i2 = GT | i1 < i2 = LT | otherwise = EQ
        where
            i1 = h1*60+m1
            i2 = h2*60+m2
--compare ()

-- Adjuk meg az isValid függvényt, mely eldönti, hogy egy idő megfelelő formátumú-e?
-- (1 <= óra <= 12 és 0 <= perc <= 59)
isValid :: USTime -> Bool
isValid (AM h m) = h <= 12 && h > 0 && m <= 59 && m >= 0
isValid (PM h m) = h <= 12 && h > 0 && m <= 59 && m >= 0

-- Készítsük el az add függvényt, mely összead két USTime-t 
-- Túlcsordulás esetén újra kezdődik AM 01 00-tól
add :: USTime -> USTime -> USTime
add a b
    | th > 24   = AM (mod th 24) tm
    | th > 12   = PM (mod th 12) tm
    | otherwise = AM th tm
    where
        th = h1 + h2 + div (m1 + m2) 60
        tm = mod (m1 + m2) 60
        m1 = snd $ convert a
        m2 = snd $ convert b
        h1 = fst $ convert a
        h2 = fst $ convert b
        convert (AM h m) = (h,m)
        convert (PM h m) = (h+12,m)



-- Írjuk meg az earliest függvényt, mely egy listából kiválasztja a legkorábbi időpont.
earliest :: [USTime] -> USTime 
earliest = minimumBy (\a b -> compare a b)

-- Akkor fog átmenni az összes teszten, ha a test változó True lesz
test :: Bool
test = and [showTest, compareTest, isValidTest, addTest, earliestTest]

-- Mindegyik fügvényünk helyességét tesztelhetjük a hozzá tartoz Test konstans kiértékelésével.


showTest     = and [show (AM 12 12) == "12:12 a.m.", show (PM 1 30) == "01:30 p.m.", 
                    show (AM 5 9) == "05:09 a.m."]
compareTest  = and [compare (AM 12 12) (AM 12 12) == EQ, compare (AM 12 10) (AM 12 12) == LT, 
                    compare (PM 10 0) (AM 10 0) == GT]
isValidTest  = and [isValid (AM 10 12), isValid (AM 12 59), not $ isValid (AM 13 00), 
                    not $ isValid (PM 00 00), not $ isValid (PM 04 60)]
addTest      = and [add (AM 5 0) (AM 5 0) == (AM 10 0), add (AM 12 59) (AM 0 1) == (PM 1 0), 
                    add (AM 5 0) (PM 1 0) == (PM 6 0), add (PM 1 0) (AM 1 0) == (PM 2 0), 
                    add (PM 1 0) (PM 1 0) == (AM 2 0)]
earliestTest = and [earliest [AM 5 0, AM 5 0, AM 10 0] == AM 5 0, 
                    earliest [PM h m | h <- [1..12], m <- [0..59]] == PM 1 0]

                    


data MyTime = T {hhour :: Int, minute :: Int, isPm :: }
    deriving (Read, Show, Eq)                    


data Lofasz = Lofasz {meret :: Int, szin :: String, vastagsag :: Int}
    deriving (Eq)

instance Show Lofasz where
    show a = '8' : (replicate (meret a) '=') ++ "B"