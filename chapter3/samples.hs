import Data.Char ( toUpper )

data Thing = Shoe  -- 5 个 data constructors
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax, King, Cabbage, King]

-- 模式匹配
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall' :: Thing -> Bool
isSmall' Ship = False
isSmall' King = False
isSmall' _    = True

data FailableDouble = Failure
                    | OK Double
  deriving Show

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Thing  -- name, age, favorite thing
  deriving Show

mike :: Person
mike = Person "Mike" 20 Ship

getAge :: Person -> Int
getAge (Person _ age _) = age

-- x @ pattern
baz :: Person -> String
baz p @ (Person name _ _) = "The name filed of (" ++ show p ++ ") is " ++ name

checkFav :: Person -> String
checkFav (Person name _ Ship) = name ++ ", you're my kind of person!"
checkFav (Person name _ _)    = name ++ ", your favorite thing is lame."

hello = case "Hello" of
  [] -> 3
  ('H' : tl) -> length tl
  _  -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d

data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt

-- 多态数据类型
data Maybe' a = Just' a
              | Nothing'

example_a :: Maybe Int -> Int
example_a (Just n) = n
example_a Nothing  = (-1)

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity s) | severity >= 50 = Just s
example_b _                                        = Nothing

-- 递归数据类型
data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 1 (Cons 2 (Cons 3 Empty))

lst2 :: List String
lst2 = Cons "Hello" (Cons "world" (Cons "!" Empty))

listProduct :: List Int -> Int
listProduct Empty      = 1
listProduct (Cons h t) = h * listProduct t

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 3 (Leaf 'z'))
