# 练习

2015 版

[作业说明](http://www.seas.upenn.edu/~cis194/spring15/hw/01-intro.pdf)

[我的代码](chapter1/hw1.hs)

---

2016 版

[作业说明](http://www.seas.upenn.edu/~cis194/fall16/hw/01-intro.html)

>Try to write small functions which perform just a single task, and then combine those smaller pieces to create more complex functions.

## Exercise 1: Traffic lights

Change the code to include the yellow light, and animate a correct sequence of traffic light signalling:

* a long green phase
* a short amber phase
* a long red phase
* a short red and amber phase1
* back to green

```Haskell
import CodeWorld

myLight :: Color -> Double -> Picture
myLight c dx = colored c (translated 0 dx (solidCircle 1))

trafficLight :: Color -> Color -> Color -> Picture
trafficLight c1 c2 c3 = myLight c1 (2.5) & myLight c2 0 & myLight c3 (-2.5) & rectangle 2.5 7.5

trafficLightAnimation :: Int -> Picture
trafficLightAnimation t 
  | t <= 2 = trafficLight black black green
  | t == 2 = trafficLight black yellow black
  | t >= 3 && t <= 4 = trafficLight red black black
  | otherwise = trafficLight red yellow black
  
exercise1 :: Double -> Picture
exercise1 t = trafficLightAnimation (round t `mod` 6)

main :: IO ()
main = animationOf exercise1
```

## Exercise 2: Blooming trees

`tree` 定义如下：

```Haskell
tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))
```

实现一个开花效果，要求在 10s 内完成，且将花的形状作为参数传入 `tree` 中，`tree` 与时间需要解耦（即时间作为花的参数存在）：

```Haskell
import CodeWorld

tree :: Integer -> Picture -> Picture
tree 0 ann = ann
tree n ann = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) ann) & rotated (- pi/10) (tree (n-1) ann))

bloom :: Int -> Picture
bloom t = colored yellow (solidCircle 0.1)

flower :: Double -> Picture
flower t = colored yellow (solidCircle ((min t 10) / 100))

animation :: Double -> Picture
animation t = tree 8 (flower t)

main :: IO ()
main = animationOf animation
```

## Exercise 3: Sokoban tiles

任务是依图画出迷宫：

```Haskell
import CodeWorld

wall, ground, storage, box :: Picture

wall = colored (gray 0.4) (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = colored brown (solidRectangle 1 1)
box = colored black (solidCircle 0.3) & ground

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = box
drawTile 4 = storage
drawTile _ = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4 || abs y > 4 = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  
drawRow :: Integer -> Integer -> Integer -> Picture
drawRow x y n
  | x >= (-n) && x <= n = translated (fromIntegral x) (fromIntegral y) 
    (drawTile (maze x y)) & drawRow (fromIntegral (x + 1)) (fromIntegral y) n
  | otherwise = blank

drawCol :: Integer -> Integer -> Integer -> Picture
drawCol x y n
  | y >= (-n) && y <= n = drawRow x y n & drawCol x (y + 1) n
  | otherwise = blank

draw :: Integer -> Picture
draw n = drawCol (-n) (-n) n

pictureOfMaze :: Picture
pictureOfMaze = draw 4

main :: IO ()
main = drawingOf pictureOfMaze
```
