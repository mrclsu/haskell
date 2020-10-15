module HanoiTower where

import Control.Monad.Writer

-- Disks of different sizes
type Disk = Int

-- A rod can have several disks on it
type Rod  = [Disk]

-- A Hanoi problem consists of three rods
type Problem = (Rod, Rod, Rod)

-- Identifier for the rods
data RodID = A | B | C
  deriving (Eq, Ord, Show)

-- Move the topmost disk from one rod to another
type Move = (RodID, RodID)

initial :: Int -> Problem
initial n = ([1..n] ,[], [])

validateRod :: Rod -> Bool
validateRod (x:y:xs) 
  | x < y = validateRod (y:xs)
  | otherwise = False
validateRod _ = True

validateProblem :: Problem -> Bool
validateProblem (a, b, c) = validateRod a && validateRod b && validateRod c

move :: RodID -> RodID -> Problem -> Problem
move A B (a:as, bs, cs) = (as, a:bs, cs)
move A C (a:as, bs, cs) = (as, bs, a:cs)
move B A (as, b:bs, cs) = (b:as, bs, cs)
move B C (as, b:bs, cs) = (as, bs, b:cs)
move C A (as, bs, c:cs) = (c:as, bs, cs)
move C B (as, bs, c:cs) = (as, c:bs, cs)
move _ _ p = p

executeMove :: Move -> Problem -> Problem
executeMove (src, dest) prob = move src dest prob

executeMoves :: [Move] -> Problem -> Problem
executeMoves moves prob = foldl (flip executeMove) prob moves

freeRod :: RodID -> RodID -> RodID
freeRod A B = C
freeRod A C = B
freeRod B C = A
freeRod B A = C
freeRod C A = B
freeRod C B = A

type SolverM = Writer [Move]
  