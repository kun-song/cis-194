{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
-- 注意 uncurry 使用
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter (uncurry (==)) $ zip xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors []       = [0, 0, 0, 0, 0, 0]
countColors (x : xs) = map (uncurry (+)) $ zip (map (fromEnum . (== x)) colors) (countColors xs)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (uncurry min) $ zip (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = let exactCounts = exactMatches secret guess in
                       Move guess exactCounts (matches secret guess - exactCounts)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = let (Move guess _ _) = move in
                         move == getMove code guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (: []) colors
allCodes n = concatMap (\x -> [x ++ [c] | c <- colors]) (allCodes (n - 1))

-- Exercise 7 -----------------------------------------
-- Given the secret:
-- 1. find all codes with the n = length of secret
-- 2. get [Move] with code and secret
-- 3. filter [Move] consistent with the given secret
-- 4. 
solve :: Code -> [Move]
solve secret = aux (allCodes (length secret)) []
               where aux [] acc = acc
                     aux (guess : guesses) acc = aux (filterCodes move guesses) (move : acc)
                                                 where move = getMove secret guess

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
