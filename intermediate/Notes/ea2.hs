{-# language KindSignatures #-}


-- Típusosztályok: bevezetés, semigroup, monoid, functor
--------------------------------------------------------

import Prelude
    hiding (Semigroup(..), Monoid(..), Functor(..), Applicative(..), Monad(..))

-- 3 osztály: Functor, Applicative, Monad
--        összesen 4 darab metódus

--------------------------------------------------------

-- példa "rossz" osztályra:

class Default a where -- (default érték tetszőleges típushoz)
    defaultVal :: a

    -- nincs rendező elv

instance Default Bool where -- arbitary choice
    defaultVal = True

instance Default Int where
    defaultVal = 0

-- Eq: rendszerező elv: minden instance egyenlőségvizsgálat legyen
--  instance Eq Bool where
--  (==) _ _ = True

--  class Convert a b where
--      convert :: a -> b

-- ehelyett: megalapozás:
--      algebrai struktúra: (típusok, múveletek, tulajdonságok)
--      (tulajdonság: konvenció, programozók közti kommunikáció)

-- pl: Eq osztály: tulajdonság: ekvivalencia-reláció
--      reflexív:       (x == x) == True
--      szimmetrikus:   ha ((x == y) == True), akkor ((y == x) == True)
--      tranzitív       stb.

-- "félcsoport"
class Semigroup a where
    infixr 4 <>
    (<>) :: a -> a -> a
    -- (<>) legyen asszociatív

instance Semigroup [a] where
    (<>) = (++)
    -- asszociatív: xs ++ (ys ++ zs) == (xs ++ ys) ++ zs

--  Két lehetőség Int-re:
--  instance Semigroup Int where
--      (<>) = (+)
--  
--  instance Semigroup Int where
--      (<>) = (*)

-- Ha több természetes instance-van akkor newtype wrappert használunk

-- data deklaráció alternatívája
newtype Sum = Sum Int
--  data Sum = Sum Int

-- különbség: nincsen futásidejű költség/reprezentáció newtype-al
--          "erősen típusozott szinoníma"

-- példa:
sum1 :: Sum
sum1 = Sum 1000

sumFun :: Sum -> Int
sumFun (Sum n) = n + 100


instance Semigroup Sum where
    Sum x <> Sum y = Sum (x + y)

newtype Prod = Prod Int

instance Semigroup Prod where
    Prod x <> Prod y = Prod (x * y)

------------------------------------------------------------

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> _       = Nothing
    _       <> Nothing = Nothing
    Just x  <> Just y  = Just (x <> y)



-- Monoid

-- adjunk egy egységelemet a semigroup-hoz (superclass megszorítás)
class Semigroup a => Monoid a where
    mempty :: a
    -- mempty <> x == x
    -- x <> mempty == x

instance Monoid [a] where
    mempty = []

instance Monoid Sum where
    mempty = Sum 0

instance Monoid Prod where
    mempty = Prod 1



-- Functor
------------------------------------------------------------

-- olyan típusok vannak Functor-ban, amelyek van "map" művelet
-- lista: map :: (a -> b) -> [a] -> [b]

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

-- típuskonstruktor: ha megadunk egy konkrét típust neki, akkor kapunk 
--      egy konkrét típust

--  Maybe       : típuskonstruktor
--  Maybe Int   : konkrét típus
--  []          : típuskonstruktor
--  [Int]       : konkrét típus
--  [] Int      : ^^ ugyanaz


list1 :: [] Int
list1 = [0..10]

maybe1 :: Maybe Int
maybe1 = Just 100

instance Functor Maybe where
    -- (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing  = Nothing
    fmap f (Just a) = Just (f a)

-- hibás:
--  instance Functor Int   -- Int nem típuskonstruktor

-- típus szintű típusozás: van
-- ghci parancs:    :k <típusszintű kifejezés>
--                  típuskifejezés "kind"-ját

-- konkrét típusok kind-ja: *
-- pl ghci-ben:
-- > :k Int
-- Int :: *

-- egyparaméteres konstruktor kind-ja: * -> *
-- Maybe :: * -> *
-- []    :: * -> *

-- két paraméter
-- Either :: * -> * -> *

-- parciális applikáció
-- Either Int :: * -> *
-- Either Int Bool :: *

-- tetszólegesen alkalmazható (->)
-- (* -> *) -> * -> *
-- ((* -> *) -> * -> *) -> *

-- data/newtype segítségével lehet valamilyen kind-ú típust definiálni
-- kapcsoljuk be: {-# language KindSignatures #-}

data Three a b c = Three a b c
-- Three :: * -> * -> * -> *

data MyData (f :: * -> *) = MyData (f Int) (f Bool)

myData :: MyData Maybe
myData = MyData (Just 10) (Just True)

myData2 :: MyData []
myData2 = MyData [10] [True, False]


def1 :: [Int] -> [Int]
def1 = map (+10) . map (+20)

def2 :: Int -> Int
def2 n = sum (map (+10) (map (+20) [0..n]))



instance Functor ((->) a) where
--  fmap :: (a -> b) -> (c -> a) -> (c -> b)
    fmap f g = \c -> f (g c)
--  fmap = (.)
