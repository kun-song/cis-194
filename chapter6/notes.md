# notes

本章参考：

* [Real World Haskell, Chapter 25: Profiling and Optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html)

## Strict evaluation

lazy evaluation 对立面是 strict evaluation：

* function arguments are completely evaluated **before** passing them to the function.

```Haskell
f x y = x + 2
```

计算 `f 10 (10 + 9)` 时，先计算 `10` 和 `10 + 9`，即使 `10 + 9` 在函数内部并未用到。

## Side effects and purity

历史上，Haskell 首先是 pure functional，同时 Haskell 设计者又想让他是 lazy 的，不久之后，设计者发现，pure + lazy 的结果是，必须禁止副作用。

但完全没有副作用的编程语言毫无实用价值，Haskell 通过 `IO a` 类型，实现既有副作用，但又防止副作用“污染” Haskell 中的纯粹部分。

## Lazy evaluation

在 lazy evaluation 策略下，evaluation of function arguments is **delayed** as long as possible。这是通过 thunk 实现的，传递给函数的任何参数都被 thunk 包裹。

lazy evaluation 可以避免不需要的计算。

## Pattern matching drives evaluation

```Haskell
f1 :: Maybe a -> [Maybe a]
f1 m = [m, m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]
```

* 虽然 `f1` 函数体也用到了参数值，但它无需了解任何 `m` 的性质即可完成计算（`f1 e` 不依赖 shape of e），因此 `f1` 不需要计算参数 `m`；
* 相对而言，`f2` 必须了解参数的结构才能完成计算（`Nothing` or `Just a`），因此 `f2` 需要计算参数；

>Thunks are evaluated only enough to allow a pattern matching to **proceed**, and no further.

例如，`f2 (safeHead [3^100 + 10, 44])` 调用中，需要计算 `safeHead [3^100 + 10, 44]` 得到 `Just 3^100 + 10`，且不会继续计算 `3^100 + 10`，因为计算到这已经“足够”支持模式匹配的执行了。

### 重点

* Expressons are only evaluated when pattern-matched
* ... only as far as necessary for the match to **proceed**, and no further!

Haskell 编译器利用 graph reduction 优化计算，每个表达式用一个 graph 表示，因此相同的子表达式可以被多个指针引用，从而减少一些重复计算。

例如 `f x = [x, x]`，`f (1 + 1)` 时，`1 + 1` 仅被计算一次，结果被两个 `x` 共享。

## Consequences of lazy

lazy 的结果：

* purity
* 短路求值操作符
* 自定义控制结构
* infinite 数据结构

### Purity

前面说过，选择 lazy evaluation，则必须选择 purity。

### Understanding space usage

lazy 导致难以推断代码的 space suage。

### 短路操作符

Java 中的 `&&`、`||` 是短路求值的，但 Java 不是 lazy 的，无法用函数实现 `&&`，因此 Java 将 `&&` 作为一种特殊的 **内置** 操作符。

Haskell 可以很容易将 `&&` 定义为普通函数：

```Haskell
(&&) :: Bool -> Bool -> Bool
True && x  = x
False && _ = False
```

也可以定义非短路求值版的 `&&`：

```Haskell
(&&!) :: Bool -> Bool -> Bool
True &&! True   = True
True &&! False  = False
False &&! True  = False
False &&! False = False
```

`&&!` 有一些很不好的结果：

* `False &&! (34^9784346 > 34987345)` 需要计算第二个参数，很久才能计算完；
* `False &&! (head [] == 'x')` 计算第二个参数时，报错；

### 自定义 control structures

比短路求值更近一步，在 Haskell 中可以轻易自定义控制结构（`if`、`while` 之类）。

```Haskell
if' :: Bool -> a -> a -> a
if True x _  = x
if False _ y = y
```

但奇怪的是 Haskell 也内置了 `if`，实际上完全可以不需要。

### Infinite 数据结构

```Haskell
nats :: [Integer]
nats = 0 : map (+ 1) nats
```

### Pipelining/wholemeal programming

## Profiling

要对 Haskell 代码调优，首先代码中需要有 `main :: IO ()`，然后使用 `-rtsopts` 选项编译，然后用 `-rtsopts` 选项运行代码，`+RTS` 之后的参数将作为运行时系统的参数传入，而非应用的参数，这些参数将控制调优过程：

* `-s` 使应用运行结束后 dump out memory and time usage information
* `-h` produce a heap profile，文件名为 `XXX.ph`
* `-i` 设置 heap profiling 的时间间隔，以秒为单位

heap profiling 文件生成后，使用 hp2ps 将其转换为 .ps 文件，可添加 `-c` 选项以生成颜色。

例如，使用 `-rtsopts` 编译：

```Haskell
ghc --make -main-is HW06 hw06.hs -rtsopts
```

使用 `-RTS` 和 `-rtsopts` 运行：

```Haskell
./hw06 +RTS -s -h -i0.001
```

转换 heap profiling 文件：

```Haskell
hp2ps -c hw06.hp
```