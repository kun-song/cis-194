# IO

本章参考：

* [Real World Haskell 第 7 章](http://book.realworldhaskell.org/read/io.html)
* [Learn You a Haskell 第 9 章](http://learnyouahaskell.com/input-and-output)

## 纯函数的局限

Haskell 3 大特点：

* functional
* pure
* lazy

其中 lazy 意味着：

* 函数不能有 **副作用**
* 函数不能依赖外部状态（键盘、文件系统、网络读写等）

但真正有用的程序必须能够与外界 IO，Haskell 通过一种特殊机制实现。

## `IO` 类型

所有可能引起副作用的计算（effectful computations）使用 `IO a` 类型表示：

* `IO a` 可能执行有副作用的 IO 操作
* 计算的结果值类型为 `a`

`IO a` 本身只是 **一个描述**，`IO a` 类型的值本身没有副作用，`IO a` 由 Haskell 执行，产生副作用的那部分被隐藏了。

>`IO a` 类似计算得到 `a` 值的菜谱，只有菜谱当然无法获得蛋糕。`IO a` 值与其他类型的值几乎没有任何区别，可以随意使用，除了 Haskell 在计算 `IO a` 时可能产生副作用。

Haskell 编译器查找 `IO a` 类型的 `main` 值：

```Haskell
main :: IO ()
```

`main` 值被 Haskell 运行时系统执行，运行时系统类似实际做蛋糕的大师傅，只有 runtime system 才被允许执行 `IO a`。

一般 `main` 由多个小 `IO a` 值组合而成，可以任意复杂。

`putStrLn`：

```Haskell
λ> :t putStrLn
putStrLn :: String -> IO ()
```

`putStrLn` 接受一个 `String` 参数，返回一个 `IO ()`，当 `IO ()` 被计算时，将参数 `String` 输出到屏幕上：

```Haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```

两种方式运行：

* `runhaskell Hello.hs`
* `ghc --make Hello.hs` 生成可执行文件，然后执行

**注意**：

GHC 将在 `Main` 模块中查找 `main` 值，而如果省略模块声明，则默认模块名字就是 `Main`（即使源文件名不是 `Main`），因此前面的 `Hello.hs` 可以正常运行。

也可以不在 `Main` 模块中查找 `main`：

```Haskell
module MyModule where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

此时 `runhaskell` 不受影响，但若要编译 Hello.hs，需要：

```Haskell
ghc --make -main-is MyModule Hello.hs
```

## `IO String` 并非容器，里面没有 `String` 值

`IO String` 看起来很像容器，似乎里面包含一个 `String` 值，但这完全是错的。

`IO String` 是一个获取 `String` 值的计算描述/菜谱，只有当该计算/菜谱被 runtime 实际执行后，结果才是 `String` 值。

## Sequecing IO actions

`do` 可用来顺序化 IO 动作：

```Haskell
module MyModule where

main :: IO ()
main = do
  putStrLn "Hello, user!"
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Hi " ++ name ++ "!"
```

* 除 `<-` 那行外，其余各行必行必须是 `IO a`；
* `do` 中各行按顺序执行；

执行：

```Haskell
λ> main
Hello, user!
What's your name?
Kyle
Hi Kyle!
```

### 一些 `IO a` 例子

#### `()` 类型

`IO ()` 中的 `()` 是一个特殊类型，可以想象为：

```Haskell
data () = ()
```

* `()` 类型只有一个值，即 `()`，它无法携带任何信息；

#### `putStrLn`

```Haskell
λ> :t putStrLn
putStrLn :: String -> IO ()
```

`puStrLn "Hello, user"` 结果为 `IO ()`，可以用于 `do` 中。

#### `getLine`

```Haskell
λ> :t getLine
getLine :: IO String
```

`getLine` 类型为 `IO String`，因此 `getLine` 被 runtime 执行后，结果为 `String`。

要获取 `getLine` 的结果，需要使用 `<-` 将结果绑定到一个名字，该名字的类型为 `String`，注意：

* `<-` 只能用于定义 IO 动作的 `do` 块；
* `name <- getLine` 本身 **没有类型**，因此它并非表达式，它只是 `do` 表达式的一部分而已；

## 复杂一点的例子

```Haskell
module MyModule where

jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 $ lines wocky
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines lines = do
  let first_line = extractFirstLines lines
  putStrLn $ unlines first_line
  return $ length first_line

extractFirstLines :: [String] -> [String]
extractFirstLines [] = []
extractFirstLines [_] = []
extractFirstLines ("" : first : rest) = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest
```

### `readFile`

`readFile` 将文件内容全部读取为 `String`：

```Haskell
λ> :t readFile
readFile :: FilePath -> IO String
```

其中 `FilePath` 是 `String` 的别名：

```Haskell
type FilePath = String
```

### `let`

`do` 中可以使用 `let`，但省略了 `in`。

### `return`

```Haskell
λ> :t return
return :: Monad m => a -> m a
```

`return` 将 pure value 转换为 IO action，注意它与 Java 中的 `return` 没有半点相似之处。

`let x = y` 与 `x <- return y` 等价，但一般用前者，因为前者明显可以看出 `y` 是“纯”的。

