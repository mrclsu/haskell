{-# LANGUAGE InstanceSigs #-}
module Bead01 where

-- Bead assignment 01:
--   Define an `Eq` instance for `T`.

data T = A Int String
       | B
       deriving (Show)

instance Eq T where
  (==) :: T -> T -> Bool
  (A x1 y1) == (A x2 y2) = x1 == x2 && y1 == y2
  B == B = True
  _ == _ = False

-----------
-- Tests --
-----------

s1, s2, s3, s4 :: T
s1 = A 0 "abcd"
s2 = A 1 "def"
s3 = B
s4 = A 1 "abcd"

tests :: [Bool]
tests = [ s1 == s1
        , s2 == s2
        , s3 == s3
        , s4 == s4
        , not $ s1 == s2
        , not $ s2 == s3
        , not $ s3 == s4
        , not $ s4 == s1
        , not $ s1 == s3
        , not $ s4 == s2
        ]

allTests :: Bool
allTests = all (== True) tests
