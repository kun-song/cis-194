{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read ( readMaybe )

data Foo = F Int | G Char

instance Eq Foo where
  (F a) == (F b) = a == b
  (G a) == (G b) = a == b
  _ == _         = False
  f1 /= f2       = not (f1 == f2)

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend' mempty'

  (<>) :: Monoid' m => m -> m -> m
  (<>) = mappend'

instance Monoid' [a] where
  mempty'  = []
  mappend' = (++)

instance Monoid' Integer where
  mempty' = 1
  mappend' = (*)

intInts :: Monoid' m => (Integer -> m) -> m
intInts make_m = aux [1 .. 100]
  where aux []       = mempty'
        aux (x : xs)
          | x `mod` 3 == 0 = mappend' (make_m x) (aux xs)
          | otherwise      = aux xs

intIntsList :: [Integer]
intIntsList = intInts (:[])

intIntsProduct :: Integer
intIntsProduct = intInts id

data Product a = Product a
instance Num a => Monoid' (Product a) where
  mempty'  = Product 1
  mappend' (Product x) (Product y) = Product (x * y)

getProduct :: Product a -> a
getProduct (Product x) = x

intIntsProduct' :: Integer
intIntsProduct' = getProduct $ intInts Product

class Functor' f where
  fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap = map

instance Functor' Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

