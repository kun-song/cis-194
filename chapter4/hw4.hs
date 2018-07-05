{-# LANGUAGE FlexibleContexts #-}
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
                            | c == 1     = "x^" ++ show e
                            | c == -1    = "-x^" ++ show e
                            | abs e == 1 = show c ++ "x"
                            | otherwise  = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p) (P q) = P (g p q)
  where g [] [] = []
        g [] ys = ys
        g xs [] = xs
        g (x : xs) (y : ys) = x + y : g xs ys

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p) (P q) = foldr plus (P [0]) $ map (P) $ t (zip [0 ..] p) q
  where t [] _ = []
        t _ [] = []
        t ((i, x) : xs) ys = (shift i $ map (* x) ys) : t xs ys

shift :: (Num a) => Int -> [a] -> [a]
shift 0 xs = xs
shift n xs = 0 : shift (n - 1) xs


-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P $ map (* (-1)) xs
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P xs) n = sum (zipWith (*) (map (\(i, v) -> v^i) $ zip [0 ..] (map (* n) $ repeat 1)) xs)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = deriv $ nderiv (n - 1) f

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P [])  = P []
    deriv (P [_]) = P []
    deriv (P cs)  = P $ tail $ map (\(i, c) -> c * (fromInteger i)) $ zip [0 ..] cs

