# Type Classes

首先在 Haskell 源文件前加：

```Haskell
{-# LANGUAGE FlexibleInstances #-}
```

这被称为 language pragma，GHC 有很多特性是标准的 Haskell 语言不具备的，可使用 language pragmas 开启这些特性。

```Haskell
import Data.Char ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read ( readMaybe )
```

Haskell 实际支持两种不同的多态，前面介绍的是：

* 参数多态（parametric polymorphism）

函数 `length :: [a] -> Int` 可用于任意类型 `a`。

但有时我们希望函数只能用于 several types，而不是 every type。例如 `+`，我们想让 `+` 可用于 `Int`、`Integer` 和 `Double`，而不能用于 `Maybe` 和 `Char`。

这种部分类型可用，但不是所有类型都能用的多态，被称为 ad-hoc polymorphism，Haskell 使用 type class 来实现 ad-hoc 多态。

A Haskell type class defines **a set of operations**. We can choose several types that support those operations via class instances.（这里的 class 和 instance 与 OO 中的毫无关系）

`Eq` type class：

```Haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

这样理解：

1. `Eq` is declared to be a type class with a single (type) parameter a.
2. Any type a which wants to be **an instance of `Eq`** must define two functions, `(==)` and `(/=)`, with the indicated type signatures.

例如，为让 `Int` 变成 `Eq` 的 instance，必须定义：

```Haskell
(==) :: Int -> Int -> Bool
(/=) :: Int -> Int -> Bool
```

看下 `(==)` 的类型：

```Haskell
(==) :: Eq a => a -> a -> Bool
```

`=>` 左侧的 `Eq a` 是 type class constraint。

对于任意类型 `a`，只要 `a` is an instance of `Eq`，`(==)` 可接受两个 `a` 作为参数，并返回 `Bool`。

>与参数多态相比，ad-hoc 多态增加了 type class constraint 约束。

当使用 `(==)` 时，编译器会利用类型推断找出 `(==)` 方的参数类型，进而找到合适的 `(==)` 实现。该动作编译时发生，因为类型在运行时是被擦除的。

例子：

```Haskell
data Foo = F Int | G Char

instance Eq Foo where
  (F a) == (F b) = a == b
  (G a) == (G b) = a == b
  _ == _         = False
  f1 /= f2       = not (f1 == f2)
```

type class 方法可以有默认实现（基于其他 type class 方法实现）：

```Haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
```

现在要声明一个 `Eq` 的实例，只需要确定 `(==)` 方法的实现就行了，会自动获取 `(/=)` 方法，当然也可以覆盖默认的 `(/=)` 实现。

实际上，标准库 `Eq` 声明如下：

```Haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (/ == y)
```

因此，实现 `Eq` instance 时，可以实现 `(==)` 或 `(/=)` 任意一个，但如果没有显示任何一个，会陷入死循环。

事实上，`Eq` type class 比较特殊，GHC 可以自动为它生成 instance：

```Haskell
data Foo = F Int | G Char
  deriving (Show, Eq, Ord)
```

`deriving` 告诉 GHC 自动为 `Show`、`Eq` 和 `Ord` 3 个 type class 生成 instance。`deriving` 默认只能用于几个 Haskell 内置的 type class：

* `Eq`
* `Enum`
* `Ord`
* `Ix`
* `Bounded`
* `Show`
* `Read`

并且并非可以为任意类型生成这些 type class 的实例。

>GHC does provide extensions that allow other classes to be derived; see the GHC manual for details.

### Type classes vs Java 接口

type class 与 Java 接口乍一看很像：都定义了一组操作，且有实例实现这些操作。但 type class 比接口更加通用：

* type class 通常会定义一组 **数学法则**，所有实例必须遵守
  + 例如 `Num` 定义的结合律、交换律
* Java 类定义时，必须先声明它实现的接口，而 type class instance 的声明与对应的 type 没有关系，甚至可以放到不同模块中
* type class method 中的类型更加灵活

关于第 3 点，考虑：

```Haskell
class Blerg a b where
  blerg :: a -> b
```

编译器需要根据 `a` 和 `b` 两个类型决定选用哪个 `blerg` 实现，而 Java 很难做到这点。

另外，Haskell 有函数依赖（Functional Dependencies）的概念，例如从容器中抽取一个元素：

```Haskell
class Extract a b | a -> b where
  extract :: a -> b
```

通过函数依赖，表明 type a uniquely determines b。

可以定义一个从 tuple 中提取第一个元素的 type class instance：

```Haskell
instance Extract (a, b) a where
  extract (x, y) = x
```

但无法定义：

```Haskell
instance Extract (a, b) b where...
```

Haskell 也更容易处理二元函数：

```Haskell
class Num a where
  (+) :: a -> a -> a
  ...
```

### 标准库 type class

* [Ord](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#t:Ord) 定义了 `<` `<=` `>` 等函数
* [Num](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#t:Num) 表示数字类型，定义加减乘等函数
* [Show](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#t:Show) 定义 `show` 函数，将值转换为字符串表示，GHC 用它输出显示
* [Read](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#t:Read) 与 `Show` 相反
* [Inegral](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#t:Integral) 表示整数，支持除法

## Monoids

类型 `m` 和操作 `(<>) :: m -> m -> m` 组成 monoid type class，且必须满足：

* 存在特殊元素 `mempty`，满足：`mempty <> x == x` 且 `x <> mempty == x`
* `<>` 满足结合律，即 `(a <> b) <> c == a <> (b <> c)`

Monoid 概念源于范畴轮，但在编程中 monoid 也随处可见（任何语言），但通过使用 type class，monoid 在 Haskell 非常显眼：

```Haskell
class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend' mempty'

  (<>) :: Monoid' m => m -> m -> m
  (<>) = mappend'
```

* `mconcat` 通过 `mappend` 和 `mempty` 定义，定义 Monoid 实例时，可以省略 `mconcat` 函数
* `<>` 与 `mappend` 等价，不过是中缀操作符，更加方便
* `GHC.base` 已经定义有 `Monoid`，所以这里用 `Monoid'`

最常见的 `Monoid` 实例可能是 list：

```Haskell
instance Monoid' [a] where
  mempty'  = []
  mappend' = (++)
```

`Monoid` 用于将两个元素结合成一个，但元素的类型，以及 **结合方式** 可能多种多样：

```Haskell
intInts :: Monoid' m => (Integer -> m) -> m
intInts make_m = aux [1 .. 100]
  where aux []       = mempty'
        aux (x : xs)
          | x `mod` 3 == 0 = mappend' (make_m x) (aux xs)
          | otherwise      = aux xs
```

* `make_m` 参数将 `Integer` 转换为 `m`。

结合方式一：可以将 `intInts` 结果作为 list：

```Haskell
intIntsList :: [Integer]
-- [3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
intIntsList = intInts (:[])
```

* `(:[])` 将 `:` 应用于空表 `[]`，与 `\x -> [x]` 等价。

结合方式二：将 `intInts` 结果以乘积方式组合：

```Haskell
intIntsProduct :: Integer
intIntsProduct = intInts id
```

`id` 类型为 `a -> a`，因此 `intIntsProduct` 需要有 `Monoid Integer` 实例。

数字类型有多种 monoidically 的组合方式，因此 Haskell 没有提供任何一种，而是通过 `Data.Monoid` 模块导出两个对数字类型的 wrapper，这两个 wrapper 都有对应的 `Monoid` 实例，`Product` 即为其中一个 wrapper：

```Haskell
data Product a = Product a
instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend (Product x) (Product y) = Product (x * y)

getProduct :: Product a -> a
getProduct (Product x) = x
```

重新定义 `intIntsProduct`：

```Haskell
intIntsProduct' :: Integer
-- 48271088561613960642858365853327381832862269440000000
intIntsProduct' = getProduct $ intInts Product
```

因为对于每个 type，只能定义一个 type class 实例，但有的 type 有多种合理的组合方式（比如 `Integer`），这些组合方式都非常有用，可以用类似 `Prodcut` 这样的包装类来避免该限制。

## Functor

```Haskell
class Functor' f where
  fmap :: (a -> b) -> f a -> f b
```

看几个 `Functor` 实例：

```Haskell
instance Functor' [] where
  fmap = map

instance Functor' Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

注意 `Functor` 的参数并非单纯的 type，而是 type constructor，即 `f` 的 kind 为 `* -> *`。

`fmap` 接受一个普通函数 `a -> b`，将它 lift 成作用在 `Functor` 上的 `f a -> f b`。

可以将 `Functor` 想象成非常通用的“容器”（`f a`），容器可以有多种 **形状**（`Option`，`Tree` 等形状不同），`fmap` 在 **不改变容器形状** 的前提下，修改容器内的元素。

例如，容器是树时，`fmap` 可以改变结点内的值，但整颗树的形状保持不变！

当不知道容器形状时，大家更喜欢用 `(<$>)` 而非 `fmap`。

>`fmap` 应用于二叉查找树时，仅保证树形状不变，但元素之间的顺序可能被影响！
