# 笔记

>第一周内容非常简单，有 FP 基础很快就可以完成，[材料地址](http://www.seas.upenn.edu/~cis194/fall16/lectures/01-intro.html)。

## 前言

Haskell 于 1980 年代末由一个学术委员会（a committee of academics）创建，那时有很多 lazy functional languages，大家各自喜好不同，交流困难，于是一堆人吸取百家之长，创造了 Haskell。

Haskell 有如下特点：

* functional
* pure
* lazy
* statically typed

<!-- more -->

### Functional

函数式（functional）并没有准确的、广泛接受的定义，当我们说 Haskell 是函数式时，我们指：

* Functions are first-class, that is, functions are **values** which can be used in exactly the same ways as any other sort of value.
* The meaning of Haskell programs is centered around **evaluating expressions** rather than executing instructions.

函数作为第一等公民 + 面向表达式编程，形成了一种新的编程范式。

### Pure

Haskell 表达式基本是引用透明（referentially transparent）的：

* No mutation!
  + Everything(variables, data structures ...) is immutable.
* Expressions never have "side effects".
  + 如更新全局变量、屏幕输出等
* Calling the same function with the same arguments results in the same output every time.
  + Programs are deterministic.

消除副作用有如下好处：

* Equational reasoning and refactoring
  + In Haskell one can always “replace equals by equals”, just like you learned in algebra class.
* Parallelism
  + Evaluating expressions in parallel is easy when they are guaranteed not to affect one another.
* Fewer headaches
  + Simply put, unrestricted effects and action-at-a-distance makes for programs that are hard to debug, maintain, and reason about.

### Lazy

在 Haskell 中，不会理解对表达式求值，而是在 **需要使用** 表达式结果时才求值。

lazy 的优点：

* 通过定义函数，很容易定义新的控制结构（control structure）
* 使无限数据结构（例如 Stream）成为可能
* It enables a more compositional programming style (see wholemeal programming below).

缺点：

* 难以推导时间、空间复杂度

### Statically typed

每个 Haskell 表达式都有类型，并且在编译时进行类型检查。

## 主题

本课程主要关注 3 个主题。

### Types

C++、Java 的静态类型系统很蛋疼，但并不意味静态类型本身很蛋疼。

Java 等语言的类型系统表现力非常差（insufficiently expressive），但 Haskell 的类型系统很吊啊：

* Helps clarify thinking and express program structure
  + 写 Haskell 程序的第一步是先写类型，因为 Haskell 的类型系统如此强大，写类型会大大澄清自己写程序的思路
* Serves as a form of documentation
  + 只看一个函数的类型，就能获取非常多信息；
  + 可通过类型搜索想要的函数，非常方便
* Turns run-time errors into compile-time errors
  + "If it compiles, it must be correct." 虽然夸大其实，但在 Haskell 中的确会有这种体会

### Abstraction

每个人都知道 DRY（Don’t Repeat Yourself）原则，思路、算法、数据、代码都不应该重复。

从相似代码中，然后重构出公共点的过程就是抽象，这是大家初学编程时都知道的概念，但不同语言对抽象的支持力度不同。

Haskell 对抽象支持非常好，参数多态、高阶函数和 type class 等特性都是为抽象服务的。

### Wholemeal programming

Ralf Hinze 说过：

>“Functional languages excel at wholemeal programming, a term coined by Geraint Jones. Wholemeal programming means to think big: work with an entire list, rather than a sequence of elements; develop a solution space, rather than an individual solution; imagine a graph, rather than a single path. The wholemeal approach often offers new insights or provides new perspectives on a given problem. It is nicely complemented by the idea of projective programming: first solve a more general problem, then extract the interesting bits and pieces by transforming the general program into more specialised ones.”

如下 C-like 代码非常常见：

```Java
int acc = 0;
for ( int i = 0; i < lst.length; i++ ) {
  acc = acc + 3 * lst[i];
}
```

我们只是要求个和而已，这段代码却要求我们关心：

1. 数组遍历的底层细节（index）
2. 混合了求和、将元素 * 3 两个完全不同的操作

在 Haskell 中只需要：

```Haskell
sum (map (3*) lst)
```

## Haskell 基础

>前面几周使用 [CodeWorld](https://code.world/haskell) 编程环境。

第一个 Haskell 程序：

```Haskell
import CodeWorld

ourPicture :: Picture
ourPicture = blank

main :: IO()
main = drawingOf ourPicture
```

### 声明与变量

以类型声明（`::` is pronounced "has type"）开始：
```Haskell
ourPicture :: Picture
ourPicture = blank
```

并将 `ourPicture` 的值定义为 `blank`，屏幕上什么都没有。

将 `blank` 替换为 `circle 1`：

```Haskell
ourPicture :: Picture
ourPicture = circle 1
```

这时屏幕显示一个圆。

注意：`ourPicture` 定义为值 `circle 1`，Haskell 禁止重复定义：

```Haskell
ourPicture = solidCircle 1
ourPucture = solidCircle 2
```

编译器提示：

```Haskell
Multiple declarations of ‘ourPicture’
```

>In Haskell, variables are not **mutable boxes**; they are just **names** for values!

Haskell 中 `=` 并非“赋值”，而是 definition（与数学上的定义相同），`ourPicture = solidCircle 1` 应该读作：

>ourPicutre is defined to be solidCircle 1.

而非：

>ourPicture gets solidCircle 1, or assign solidCircle 1 to ourPicture.

### Picture 类型与函数

与其他 Haskell 入门课不同，本课程以 `Picture` 类型开始，而非 number、boolean、tuple 等。

#### 原始 `Picture`

每个图片都是 `Picture` 类型的 **值**，例如 `blank` 和 `solidCircle 1`。

`solidCircle` 的类型为：

```Haskell
solidCircle :: Double -> Picture
```

`->` 表明 `solidCircle` 是一个函数，它将 `Double` 类型的值转换为 `Picture` 类型的值。

Haskell 中简单参数可以直接省略括号，但如果参数 **不是简单的变量名字**，则需要括号：

```Haskell
ourPicture = solidCircle (1 + 1)
```

#### 修改 `Picture`

可以用 `colored` 对 `Picture` 上色：

```Haskell
colored :: Color -> Picture -> Picture
```

两个 `->` 表明 `colored` 需要两个参数，调用时用 ` ` 隔开：

```Haskell
ourPicture = colored green (solidCircle 1)
```

>至于 `colored` 类型为啥不是 `Color Picture -> Picture`，后面会解释（柯里化）。

#### 组合 `Picture`

将两个 `Picture` 值组合成一个 `Picture` 值的函数类型应该是：

```Haskell
Picture -> Picture -> Picture
```

前面说过，在 Haskell 中可以通过函数类型 **搜索** 函数，而实际上，我们的库中符合以上类型的函数只有一个：

```Haskell
(&) :: Picture -> Picture -> Picture
```

`&` 是一个奇怪的函数名字，既可以将其用作普通函数（将参数放在右边），也可以将其用作操作符：

```Haskell
ourPicture = colored green (solidCircle 1) & solidCircle 2
```

`&` 默认将两个 `Picture` 放在同一圆心，左侧 `Picture` 在右侧 `Picture` 之上。

`colored` 只作用在 `(solidCircle 1)` 上，而不是 `(solidCircle 1) & solidCircle 2` 上，从中可以得到一个重要 Haskell 语法：

>Function application binds tighter than any binary operators.

可以通过 `translated` 函数指定 `Picture` 绘制的位置：

```Haskell
translated :: Double -> Double -> Picture -> Picture
```

利用 `translated` 可避免 `Picture` 重叠：

```Haskell
ourPicture = colored green (translated 0 (-1.5) (solidCircle 1)) & colored red (translated 0 (1.5) (solidCircle 1))
```

代码太长，将其分为组件：

```Haskell
redLight = colored red (translated 0 (1.5) (solidCircle 1))
greenLight = colored green (translated 0 (-1.5) (solidCircle 1))
frame = rectangle 2.5 5.5

ourPicture :: Picture
ourPicture = redLight & greenLight & frame
```

### 定义函数

实际上，`something = this and that` 就定义了一个函数 `something`，只不过它恰好没有任何参数；若想让函数接受参数，将参数写在 `=` 左侧即可：

```Haskell
topLight c = colored c (translated 0 (1.5) (solidCircle 1))
botLight c = colored c (translated 0 (-1.5) (solidCircle 1))
frame = rectangle 2.5 5.5

ourPicture :: Picture
ourPicture = topLight red & botLight green & frame
```

正常情况下，红灯、绿灯不会同时亮，因此设计 `trafficeLight` 函数，它只会显示红、绿灯两者其一，它的参数类型为 `Bool`：

```Haskell
topLight c = colored c (translated 0 (1.5) (solidCircle 1))
botLight c = colored c (translated 0 (-1.5) (solidCircle 1))
frame = rectangle 2.5 5.5

trafficLight True = topLight black & botLight green & frame
trafficLight False = topLight red & botLight black & frame
```

使用：

```Haskell
ourPicture = trafficLight False  -- or trafficLight True
```

一般需要显式指出 `trafficLight` 的类型：

```Haskell
trafficLight :: Bool -> Picture
```

在 FP 语言中进行抽象（abstract）要更容易。

### 动画

什么是动画？动画是随时间不断变化的 `Picture`。

命令式语言一般提供 `getCurrentTime()` 函数，我们绘制 `Picture` 时调用该函数获取时间。

在 FP 语言中，禁止（不推荐）这种做法，因为它会 **隐藏副作用**，FP 语言一般将时间作为函数参数显式提供：

```Haskell
trafficController :: Double -> Picture
trafficController t
  | round(t / 3) `mod` 2 == 0 = trafficLight True
  | otherwise = trafficLight False

main :: IO()
main = animationOf trafficController
```

* 3s 闪一次
* `drawingOf` 改为 `animationOf`，它的参数是 `Double -> Picture`，并自动传入当前时间
* `|` 形成 guard，调用 `trafficController` 时，将按照 guard 顺序，自上往下尝试，直接遇到 guard 计算即为 `True` 的分支，该分支即 `trafficController` 的代码
* 使用 backticks 将 `mod` 变为中缀操作符

### 数字类型

先介绍 3 中数字类型：`Int`、`Integer` 和 `Double`。

`Int` 与机器相关，范围至少是 -2^29 -> 2^29，但具体范围取决于机器：

```Haskell
i :: Int
i = -42
```

`Integer` 只与机器 **内存** 大小相关，一般范围远远大于 `Int`：

```Haskell
n :: Integer
n = 1111111111111111111111111111111111111111111111111111111
```

双精度浮点数 `Double`（单精度 `Float`）：

```Haskell
d :: Double
d = 6.2e-4
```

数字相关的函数有：

* `+`、`-`、`*` 可用于任意数字类型
* `/` 仅用于浮点数
* `div`、`mod` 仅用于整数
* 还有 `sin`、`cos`、`log` 等函数

Haskell 不允许隐式类型转换，因此需要：

* `fromIntegral`：将 `Int` 或 `Integer` 转换到其他数值类型
* `round`、`floor` 和 `ceiling`：将浮点数转换为 `Int` 或 `Integer`

### 递归

递归在 FP 中非常常见：

```Haskell
lights :: Int -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n - 1))

ourPicture = lights 3
main = drawingOf ourPicture
```

将复制的 `Picture`、数量、右移距离抽象出来：

```Haskell
spread :: Picture -> Double -> Int -> Picture
spread pic dx 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n - 1))
```

* `dx`：信号灯之间的偏移距离
* `n`：重复个数

编译器会提示：

```Haskell
Defined but not used: ‘dx’
```

因为第一个分支中没有用到 `dx` 和 `pic`，此时，用 `_` 替换它们：

```Haskell
spread :: Picture -> Double -> Int -> Picture
spread _ _ 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n - 1))
```

### 注释

Haskell 支持两种注释：

```Haskell
-- 单行注释

{-
   多行注释
-}
```
