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

