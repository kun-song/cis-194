# Polymorphism and Functional Programming Paradigms

本周 [材料链接](http://www.seas.upenn.edu/~cis194/spring15/lectures/02-lists.html)

## 更多语法

下面介绍局部变量。

### `let` 表达式

`let ... in` 用于定义 **表达式级别** 的局部变量：

```Haskell
strLength :: String -> Int
strLength []       = 0
strLength (_ : xs) = let tail_len = strLength xs in
                     tail_len + 1
```

### `where` 子句

`where` 用于定义 **跨多个守卫分支** 的局部变量：

```Haskell
frob :: String -> Char
frob [] = 'a'  -- len 此处不可见
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str
```

`where` 子句定义的 `len` 局部变量只对第二个分支可见，第一个分支不可见。

Haskell 代码中 `where` 比 `let` 更常见，因为 `where` 让程序员专注于函数本身的设计，而不是先定义一堆局部变量。

### Haskell 布局

Haskell 代码对 whitespace 敏感（与 Python 类似），这与其他语言差别较大。Haskell 通过缩进决定代码层次。

// TODO

### 累加器

对列表求和，如果和大于等于 20 则停止，借助辅助函数实现：

```Haskell
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums
  where go :: Int -> [Int] -> Int
        go acc []       = acc
        go acc (x : xs)
          | acc >= 20 = acc
          | otherwise = go (acc + x) xs

sumTo20 [4,9,10,2,8] == 23
```

* `go` 是辅助函数，把累加结果放到 `go` 的参数中，与以前 Scala 一模一样

## 参数多态

多态函数要求用户调用时指定具体类型，因此必须在多种类型下正常工作。而 Haskell 没有类似 `isInstanceOf` 这类类型判断的方式。

例如：

```Haskell
bogus :: [a] -> Bool
bogus ('X' : _) = True
bogus _         = False
```

不是合法的多态函数，虽然声明的类型是多态 `[a]`，但实现中假设输入是 `[Char]`。

下面是真正的多态函数：

```Haskell
notEmpty :: [a] -> Bool
notEmpty (_ : _) = True
notEmpty []      = False
```

parametricity 带来的后果之一是：类型擦除（type easure），Haskell 代码运行时根本不会使用类型信息（因为 Haskell 没有提供类似 `isInstanceOf` 之类的在运行时利用类型的方法），因此编译后即可擦除所有类型信息。

写 Haskell 代码时类型非常重要，但运行时 **完全不需要** 类型！

而类似 Python 这样的动态语言，却需要在运行时 **保持类型**，因此 Haskell 运行时效率比 Python 高得多。

parametricity 另一个后果它限制了多态函数的 **实现方式**。

例如：

```Haskell
strange :: a -> b
```

`strange` 接受 `a` 产生 `b`，但实际上并不知道 `a` 和 `b` 之间的关系，因此根本无法实现 `strange`：

```Haskell
strange = error "impossible"
```

* `error` 定义在 `Prelude` 中，停止程序

而：

```Haskell
limited :: a -> a
```

`limited` 接受 `a` 产生 `a`，实际上它只能产生它接受的 `a` 入参，因为根本不知道 `a` 有啥性质，所以不可能根据入参，算出另一个结果然后返回，唯一的实现方式就是原封不动返回入参：

```Haskell
limited x = x
```

根据 parametricity，仅从函数参数就能获取到很多信息，按图索骥：

1. 函数什么类型的值？
2. 这些值如何计算得到？
...

## total function vs partial function

考虑函数类型：

```Haskell
[a] -> a
```

Prelude 中的 `head` 即符合该签名，但 `head` 用于 `[]` 时会崩溃：

```Haskell
head []
```

毕竟 `[]` 中什么都没有，`head` 也不能无中生有。

`head` 就是所谓的 partial function：有部分输入参数是 `head` 无法处理的（`[]`）。而可以处理所有输入的函数被称为 total function。

Haskell 推荐尽量避免使用 partial function！

`head` 函数是个意外，根本不应该出现在 Prelude 中，类似的还有 `tail`、`init`、`last` 和 `!!` 等。

>作业中出现这些 partial function 会扣分！

### 替换 partial function

诸如 `head`、`tail` 之类的 partial function 可以用模式匹配替代。

例如求 list 前两个元素之和：

```Haskell
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []  = 0
doStuff2 [_] = 0
doStuff2 (x : y : _) = x + y
```

`doStuff1` 和 `doStuff2` 都是 total function，但第二个更加明显。

## 递归模式

对于列表 `[a]` 能做些什么呢？

* 对 list 中每个元素做一些操作
* 保留 list 中部分元素，去掉其他元素
* "summarize" list 中的所有元素（求和、求乘积、最大值 ...）

### Map

对 list 中每个元素 + 1：

```Haskell
addOneToAll :: [Int] -> [Int]
addOneToAll []       = []
addOneToAll (x : xs) = x + 1 : addOneToAll xs
```

对所有元素求绝对值：

```Haskell
absAll :: [Int] -> [Int]
absAll []       = []
absAll (x : xs) = abs x : absAll xs
```

对每个元素求平方：

```Haskell
squareAll :: [Int] -> [Int]
squareAll []       = []
squareAll (x : xs) = x ^ 2 : squareAll xs
```

Don't repeat yourself，将差异点抽离成参数：

```Haskell
map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs
```

前面 3 个函数都可以用 `map'` 实现：

```Haskell
map' (+ 1) [1, 2, 3]
map' abs [1, 2, 3]
map' (^ 2) [1, 2, 3]
```

### Filter

只保留 `[Int]` 中的正数：

```Haskell
keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x : xs)
  | x > 0     = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs
```

只保留 `[Int]` 中的偶数：

```Haskell
keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x : xs)
  | even x    = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs
```

将断言抽象出来：

```Haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

### Fold

有一种常见的 list 操作：

>“Combine” the elements of the list into a final answer.

例如：

```Haskell
sum' :: [Int] -> Int
sum' []       = 0
sum' (x : xs) = x + sum xs

product' :: [Int] -> Int
product' []       = 1
product' (x : xs) = x * product' xs

length' :: [Int] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs
```

抽象成 `fold`：

```Haskell
foldl' :: b -> (b -> a -> b) -> [a] -> b
foldl' b _ [] = b
foldl' b f (a : as) = foldl' (f b a) f as

foldr' :: b -> (a -> b -> b) -> [a] -> b
foldr' b _ [] = b
foldr' b f (a : as) = f a (foldr' b f as)
```

前面 3 个函数都可以用 `fold` 定义：

```Haskell
sum'' = foldl' 0 (+)
product'' = foldl' 1 (*)
length'' = foldl' 0 addOne
  where addOne acc _ = acc + 1
```

实际上 Prelude 中有很多函数是通过 `fold` 定义的：

* `length`
* `sum`
* `product`
* `and`
* `or`
* `any`
* `all`

## 函数式编程

下面介绍几个“更加函数式”的例子。

### 函数式组合子

下面介绍几个有用的组合子。

Prelude 定义了用于函数组合的组合子 `.`：

```Haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

若要把 list 中每个元素 + 1，然后 * 4，可以：

```Haskell
add1Mul4 :: [Int] -> [Int]
add1Mul4 xs = map ((* 4) . (+ 1)) xs
```

还有方便的 `$` 组合子：

```Haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

因为 `$` 被解析成操作符，因此调用时可以避免括号，所以：

```Haskell
negateEvens1 :: [Int] -> Int
negateEvens1 xs = negate (length (filter even xs))
```

可以改写成：

```Haskell
negateEvens :: [Int] -> Int
negateEvens xs = negate $ length $ filter even xs
```

### lambda

匿名函数，or lambda 表达式。

如下：

```Haskell
duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x
```

可以用匿名函数替换 `dup`：

```Haskell
duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)
```

`\` 将 `x` 变量绑定到 `->` 后面的表达式中。

## 柯里化、部分应用

一个惊人的事实：

>all functions in Haskell take only one argument.

例如：

```Haskell
f :: Int -> Int -> Int
f x y = 2*x + y
```

`f` 只有一个 `Int` 参数，但它返回一个 `Int -> Int` 函数，用括号更清晰：

```Haskell
f :: Int -> (Int -> Int)
f x y = 2*x + y
```

因为函数箭头 `->` 右结合，所以括号可以省略。

函数应用（function application）**左结合**，即 `f 2 3` 是 `(f 2) 3` 的缩写，这也清晰表明 `f` 只能接受一个参数，并返回一个函数。

因为函数应用的左结合，`(f 2) 3` 可以简写为 `f 2 3`，给人一种 `f` 是“多参函数”的假象。 

匿名函数：

```Haskell
\x y z -> ...
```

实际是如下的语法糖：

```Haskell
\x -> \y -> \z -> ...
```

类似，函数定义：

```Haskell
f x y z = ...
```

是下面的语法糖：

```Haskell
f = \x -> \y -> \z -> ...
```

将多参函数表示为“返回函数的单参函数**的思想，被称为柯里化（currying）。

可以用 tuple 表示“真正”的多参函数：

```Haskell
f :: (Int, Int) -> Int
f (x, y) = 2 * x + y
```

虽然 `f` 实际也只有一个参数，但因为该参数是 tuple，可以放两个元素，因此可以把 `f` 视为“多参函数”。

目前看“多参函数”实际有两种表示：

* 以 tuple 为参数的单参函数
* 柯里化函数

为在两者之间转换，标准库提供了 `curry` 和 `uncurry` 函数：

```Haskell
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y
```

`uncurry` 特别有用，因为它可以把普通函数用于 tuple：

```Haskell
uncurry (+) (1, 3)  -- 4
```

### 部分应用

因为 Haskell 函数都是柯里化的，因此很容易部分应用。

部分应用：函数调用（应用）时，仅提供部分参数，应用结果是一个函数，该函数接受剩余参数作为参数。

Haskell 中，很容易部分应用 **第一个** 参数，但其余参数却不容易部分应用，实际使用中这没啥大问题，因为函数设计时需要根据参数变化的可能性合理决定 **参数顺序**：

* 不经常变的靠左
* 经常变的靠右

>中缀操作符是个例外，它的两个参数都很容易部分应用。

### Wholemeal programming

如下：

```Haskell
foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x : xs)
  | x > 3     = (7 * x + 2) + foobar xs
  | otherwise = foobar xs
```

`foobar` 不符合 Haskell 设计风格：

* doing too much at once, and
* working at too low of a level

不要考虑应该如何处理 **每个元素**，尝试考虑如何对 **整个** 列表进行 **转换**，得到如下更地道的 Haskell 代码：

```Haskell
foobar' :: [Integer] -> Integer
foobar' = sum . map (( + 2) . (* 7)) . filter (> 3)
```

`foobar'` 是由 3 个函数组成的流水线：`filter` + `map` + `sum`。

注意 `foobar'` 实现中，`map` 和 `filter` 都被部分应用，然后用 `.` 进行函数组合。

### Point-free Style

前面 `foobar'` 定义中，没有涉及函数参数：

```Haskell
foobar' = sum . map (( + 2) . (* 7)) . filter (> 3)
```

这种风格被称为 point-free 风格，合理使用，可以实现非常优美的函数。

但过度追求 point-free 则会导致晦涩难懂的代码：

```Haskell
@pl \f g x y -> f (x ++ g x) (g y)
join . ((flip . ((.) .)) .) . (. ap (++)) . (.)
```

haskell IRC 有一个 `@pl` 可以把函数转换为 point-free 风格，上面代码转换后明显很难懂。

再看一个例子，下面两个函数与 `map` 等价：

```Haskell
mumble  = (`foldr` []) . ((:).) 
grumble = zipWith ($) . repeat
```
