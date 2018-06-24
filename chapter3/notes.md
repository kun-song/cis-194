# Algebraic data types

建议阅读：

* [Real World Haskell](http://book.realworldhaskell.org/read/), chapters 2 - 4.

## 库

Haskell 的 libray 是以 package 发布的，每个 package 可以包含任意数量的 module。

安装 Haskell 时也会安装 base package，base 包中包含 Prelude module，它里面的 definition 自动引入所有 Haskell 代码中。base 包中的其他模块可以手动导入：

```Haskell
import Data.Char ( toUpper )
```

`()` 可以省略，省略则导入 `Data.Char` 中的所有定义。

类似 maven 和 sbt，Haskell 有自己的包管理器：cabal，也有自己的包服务器：Hackage。安装 Haskell 时自动安装 cabal：

```Haskell
cabal update
cabal install text-1.1.1.3
```

## 枚举类型

创建：

```Haskell
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show
```

以上声明一个新类型 `Thing` 以及创建 `Thing` 的 5 个 data constructor：`Shoe`、`Ship` 等。

通过 data constructor 创建 `Thing` 类型的值：

```Haskell
shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax, King, Cabbage, King]
```

模式匹配：

```Haskell
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False
```

简化：

```Haskell
isSmall' :: Thing -> Bool
isSmall' Ship = False
isSmall' King = False
isSmall' _    = True
```

## 超越枚举

`Thing` 是枚举不假，与 Java 中的枚举类型。

但枚举实际是 algebraic data type 的特例而已，因此 Haskell 枚举可不仅仅是枚举：

```Haskell
data FailableDouble = Failure
                    | OK Double
  deriving Show
```

`FailableDouble` 类型有两个 data constructors：

* `Failure` 是第一个构造器，但它没有参数，因此：
  + `Failure` **本身** 就是 `FailableDouble` 类型的一个 **值**；
* 第二个构造器 `OK`，接受一个 `Double` 类型的参数，因此：
  + `OK` 本身不是 `FailableDouble` 类型的值
  + `OK 2.4` 才是 `FailableDouble` 的值

```Haskell
a = Failure
b = OK 2.4
```

>`OK` 类型为 `Double -> FailableDouble`。

看 `OK` 如何用于模式匹配：

```Haskell
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d
```

data constructor 参数可以为多个：

```Haskell
data Person = Person String Int Thing  -- name, age, favorite thing
  deriving Show

mike :: Person
mike = Person "Mike" 20 Ship

getAge :: Person -> Int
getAge (Person _ age _) = age  -- 注意 = 左侧格式！
```

>**注意**：这里 type constructor 和 data constructor 都叫 `Person`，但它们在不同的名字空间中，是完全不同的东西。
>
>在 Haskell 中，若只有一个 data constructor，则让 type constructor 和 data constructor 取同样的名字很常见。

## Algebaric data types in general

一个 ADT 通常有多个 data constructors，每个 data constructor 可以有 0 或多个参数：

```Haskell
data AlgDataType = Constructor1 Type11 Type12
                 | Constructor2 Type21
                 | Constructor3 Type31 Type32 Type33
                 | Constructor4
```

以上表明 `AlgDataType` 的值可以通过 4 中方式构造：`Constructor1` ... `Constructor4`。

根据构造器的不同，`AlgDataType` 的值可能包含 **其他值**，例如若通过 `Constructor1` 构造，则会包含两个值，类型分别是 `Type11` 和 `Type12`。

另外，构造器与变量的名字规范不同：

* type constructors 和 data constructors 必须以 **大写** 字母开头；
* variable(including names of functions) 必须以 **小写** 字母开头；

该规范的目的是简化 Haskell 的解析任务。

## 模式匹配

前面看了几个模式匹配的特殊例子，下面详细介绍模式匹配的工作原理。

Pattern-matching is about **taking apart a value** by finding out **which constructor* it was built with.

目的：

* 拆解值（take apart a value）

实现方式：

* 找到该 value 是哪个 data constructor 构建的

This information can be used as the basis for deciding what to do—indeed, in Haskell, this is the **only** way to make a decision.

例如，要决定如何处理一个 `AlgDataType` 类型的 value，可以：

```Haskell
foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...
```

* 匹配 constructor 的同时，也解析出了构造器使用的参数；
* 构造器有参数时，需要加括号；

#### 通配符

`_` 可以匹配 anything。

#### `x @ pattern`

`x @ pattern` 将 value 与 `pattern` 模式匹配，若匹配成功，则将 value 绑定到 `x` 名字：

```Haskell
baz :: Person -> String
baz p @ (Person name _ _) = "The name filed of (" ++ show p ++ ") is " ++ name
```

输出 `"The name filed of (Person \"Mike\" 20 Ship) is Mike"`

#### 模式可以嵌套

```Haskell
checkFav :: Person -> String
checkFav (Person name _ Ship) = name ++ ", you're my kind of person!"
checkFav (Person name _ _)    = name ++ ", your favorite thing is lame."
```

#### 其他

常见模式如下：

```Haskell
pat ::= _
     |  var
     |  var @ ( pat )
     |  ( Constructor pat1 pat2 ... patn )
```

* 通配符 `_` 匹配 anything
* 变量匹配 anything
* `x @ pattern`
* constructor 模式

**注意**：2 或 `c` 都可以被视为 **没有参数的 constructor**，可以把 `Int` 和 `Char` 想象成（当然实际不是这样！）：

```Haskell
data Int = 0 | 1 | -1 | 2 | -2 | ...
data Char = 'a' | 'b' | 'c' | ...
```

## Case expression

Haskell 中用来做模式匹配的 construct 是 case 表达式：

```Haskell
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
```

表达式 `exp` 将被 **自上而下** 与 `pat1` `pat2` ... 匹配，例如：

```Haskell
hello = case "Hello" of
  [] -> 3
  ('H' : tl) -> length tl
  _  -> 7
```

结果为 4。

前面的函数定义实际都是 case 表达式的语法糖，例如 `failureToZero`：

```Haskell
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d
```

实际是如下 case 表达式的语法糖：

```Haskell
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d
```

## 多态数据类型

本周作业中的几个数据类型：

```Haskell
data LogMessage = LogMeesage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt
```

后两个结构几乎完全相同，可抽象为：

```Haskell
data Maybe a = Just a
             | Nothing
```

* Prelude 定义了 `Maybe`

`Maybe a` 类型表示可能有 `a` 的值，也可能没有。`Maybe` 本身不是具体类型（well-formed type），它接受一个具体类型 `a`，然后产生类型 `Maybe a`，因此 `Maybe` 被称为 type constructor 或 parameterized type。

因此 `MaybeLogMessage` 可以替换为 `Maybe LogMessage`，`MaybeInt` 可替换为 `Maybe Int`。

### 高阶类型：kind

介绍了 type constructor 后，有必要介绍 type of type 的概念，this is called a kind.

* Haskell 中所有 well-formed type（`Int` 等）的 kind 为 `*`
* 接受一个类型参数的 type constructor（如 `Maybe`）的 kind 为 `* -> *`

而 `Maybe Int` 类型的 kind 为 `*`，因为提供 `Int` 后，它已经是 well-formed 的类型了。

类型注释中的类型的 kind 必须为 `*`，因此 `Int -> Maybe` 是错误的函数类型。

```Haskell
example_a :: Maybe Int -> Int
example_a (Just n) = n
example_a Nothing  = (-1)

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity s) | severity >= 50 = Just s
example_b _                                        = Nothing
```

## 递归数据类型

data type 可以是递归的，例如：

```Haskell
data List t = Empty | Cons t (List t)
```

给定类型 `t`，`List t`：

* 要么是构造器 `Empty`
* 要么是构造器 `Cons`，但 `Cons` 构造器第二个参数又是另一个 `List t`（递归）

```Haskell
lst1 :: List Int
lst1 = Cons 1 (Cons 2 (Cons 3 Empty))

lst2 :: List String
lst2 = Cons "Hello" (Cons "world" (Cons "!" Empty))
```

我们定义的 `List` 与内置的毫无区别，除了内置类型有一些特殊的语法糖，例如 `[Int]`，实际上是 `[] Int`。

递归的数据类型，一般用递归函数处理：

```Haskell
listProduct :: List Int -> Int
listProduct Empty      = 1
listProduct (Cons h t) = h * listProduct t
```

再例如一棵树：

```Haskell
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 3 (Leaf 'z'))
```
