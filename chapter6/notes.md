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

