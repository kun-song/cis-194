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
