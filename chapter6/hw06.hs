{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = f 0
  where f n = fib n : f (n + 1)

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x s) = Cons (f x) $ fmap f s

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f $ f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s1) s2 = Cons x $ sInterleave s2 s1

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 $ (+ 1) <$> nats

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) $ (+ 1) <$> ruler

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons seed $ f <$> rand seed
  where f old_seed = (1103515245 * old_seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = aux Nothing xs
  where aux acc []                 = acc
        aux Nothing (y : ys)       = aux (Just (y, y)) ys
        aux (Just (n, m)) (y : ys) = aux (n `seq` m `seq` Just (n `min` y, m `max` y)) ys

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
