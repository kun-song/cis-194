-- 表达式级别的局部变量：let
strLength :: String -> Int
strLength [] = 0
strLength (_ : xs) = let tail_len = strLength xs in
                     tail_len + 1

-- 分支级别的局部变量：where
frob :: String -> Char
frob [] = 'a'  -- len 此处不可见
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- 局部辅助函数
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums
  where go :: Int -> [Int] -> Int
        go acc []       = acc
        go acc (x : xs)
          | acc >= 20 = acc
          | otherwise = go (acc + x) xs

notEmpty :: [a] -> Bool
notEmpty (_ : _) = True
notEmpty []      = False

-- 无法实现类型 a -> b 的函数
strange :: a -> b
strange = error "impossible"

-- a -> a 类型的函数只有一种实现方式：直接返回入参
limited :: a -> a
limited x = x

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []  = 0
doStuff2 [_] = 0
doStuff2 (x : y : _) = x + y

addOneToAll :: [Int] -> [Int]
addOneToAll []       = []
addOneToAll (x : xs) = x + 1 : addOneToAll xs

absAll :: [Int] -> [Int]
absAll []       = []
absAll (x : xs) = abs x : absAll xs

squareAll :: [Int] -> [Int]
squareAll []       = []
squareAll (x : xs) = x ^ 2 : squareAll xs

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x : xs)
  | x > 0     = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x : xs)
  | even x    = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

sum' :: [Int] -> Int
sum' []       = 0
sum' (x : xs) = x + sum xs

product' :: [Int] -> Int
product' []       = 1
product' (x : xs) = x * product' xs

length' :: [Int] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs

foldl' :: b -> (b -> a -> b) -> [a] -> b
foldl' b _ [] = b
foldl' b f (a : as) = foldl' (f b a) f as

foldr' :: b -> (a -> b -> b) -> [a] -> b
foldr' b _ [] = b
foldr' b f (a : as) = f a (foldr' b f as)

sum'' = foldl' 0 (+)
product'' = foldl' 1 (*)
length'' = foldl' 0 addOne
  where addOne acc _ = acc + 1

add1Mul4 :: [Int] -> [Int]
add1Mul4 xs = map ((* 4) . (+ 1)) xs

negateEvens1 :: [Int] -> Int
negateEvens1 xs = negate (length (filter even xs))

negateEvens :: [Int] -> Int
negateEvens xs = negate $ length $ filter even xs

duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x : xs)
  | x > 3     = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (( + 2) . (* 7)) . filter (> 3)
