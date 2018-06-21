{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
  | n < 10    = n
  | otherwise = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n
  | n < 10    = 0
  | otherwise = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n = (lastDigit n) : (toRevDigits (dropLastDigit n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []           = []
doubleEveryOther (x : [])     = [x]
doubleEveryOther (x : y : xs) = x : (y * 2) : (doubleEveryOther xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : xs) = sum (toRevDigits x) + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits (doubleEveryOther (toRevDigits n))) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ (a, b) : (hanoi (n - 1) c b a)

hanoi2 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ = []
hanoi2 n a b c = (hanoi2 (n - 1) a c b) ++ ((a, c) : hanoi2 (n-1) b a c)
