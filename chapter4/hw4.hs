{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P []) == (P []) = True
    (P []) == (P (y : ys))
      | y == 0    = (P []) == (P ys)
      | otherwise = False
    (P (z : zs)) == (P [])
      | z == 0    = (P zs) == (P [])
      | otherwise = False
    (P (z : zs)) == (P (y : ys))
      | z == y    = (P zs) == (P ys)
      | otherwise = False

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) = concat $ intersperse " + " $ reverse $ map f $ filter (\(_, b) -> b /= 0) $ zip [0 ..] xs
                    where f (0, c)  = show c
                          f (1, 1)  = "x"
                          f (1, -1) = "-x"
                          f (e, c)
                            | abs e == 1 = show c ++ "x"
                            | otherwise  = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

