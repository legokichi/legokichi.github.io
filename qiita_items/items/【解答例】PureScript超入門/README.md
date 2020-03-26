
[Haskell 超入門](http://qiita.com/7shi/items/145f1234f8ec2af923ef)の問題をPureScript 0.7 でやってみました。
[【解答例】Haskell 超入門](http://qiita.com/7shi/items/0ece8c3394e1328267ed)のHaskellでの解答例と並べて比べてみましょう。

## フィボナッチ数
> 【問1】任意のn番目のフィボナッチ数を計算する関数fibをパターンマッチで実装してください。
> 【問2】問1で実装した関数をガードで書き直してください。
> 【問3】問1で実装した関数を`case-of`で書き直してください。

```hs:Haskell
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n - 2) + fib (n - 1)
```

```hs:Haskell
fib n
    | n == 0 = 0
    | n == 1 = 1
    | n >  1 = fib (n - 2) + fib (n - 1)
```

```hs:Haskell
fib n = case n of
    0 -> 0
    1 -> 1
    _ | n > 1 -> fib (n - 2) + fib (n - 1)
```

```hs:PureScript
module Q1 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

-- | using patern match
fib :: Int -> Int
fib n
  | n == 0 = 1
  | n == 1 = 1
  | n >  1 = fib (n - 2) + fib (n - 1)

-- | using guards
fib2 :: Int -> Int
fib2 n
  | n == 0 = 1
  | n == 1 = 1
  | n >  1 = fib2 (n - 2) + fib2 (n - 1)

-- | using case of
fib3 :: Int -> Int
fib3 n = case n of
    0 -> 1
    1 -> 1
    _ | n > 1 -> fib3 (n - 2) + fib3 (n - 1)

-- | > Q1.main
-- | 89
-- | 89
-- | 89
-- | unit
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ fib 10
  print $ fib2 10
  print $ fib3 10
```

ここまでは順調ですね。


# 再実装
> 【問4】`sum`, `product`, `take`, `drop`, `reverse`と同じ機能の関数を再実装してください。関数名には'を付けてください。

```hs:Haskell
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' []     = 1
product' (x:xs) = x * product' xs

take' _ []        = []
take' n _ | n < 1 = []
take' n (x:xs)    = x : take' (n - 1) xs

drop' _ []         = []
drop' n xs | n < 1 = xs
drop' n (_:xs)     = drop' (n - 1) xs

reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
```

```hs:PureScript
module Q2 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Array
import Data.Maybe
import Data.Int

sum' :: forall a. (Ring a)=> Array a -> a
sum' arr = case uncons arr of
  Just { head: x, tail: xs } -> x + sum' xs
  Nothing -> zero

product' :: forall a. (Ring a)=>  Array a -> a
product' arr = case uncons arr of
  Just { head: x, tail: xs } -> x * product' xs
  Nothing -> one

take' :: forall a. Int -> Array a -> Array a
take' 0 arr = []
take' n arr = case uncons arr of
  Just { head: x, tail: xs } -> x : take' (n - 1) xs
  Nothing -> []

drop' :: forall a. Int -> Array a -> Array a
drop' 0 arr = arr
drop' n arr = case uncons arr of
  Just { head: x, tail: xs } -> drop' (n - 1) xs
  Nothing -> []

reverse' :: forall a. Array a -> Array a
reverse' arr = case uncons arr of
  Just { head: x, tail: xs } -> (reverse' xs) `snoc` x
  Nothing -> []

-- | > Q2.main
-- | 15
-- | 120
-- | [3]
-- | [1,2]
-- | [5,4,3,2,1]
-- | unit
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ show $ sum' $ range 1 5
  print $ product' $ range 1 5
  print $ drop' 2 [1, 2, 3]
  print $ take' 2 [1, 2, 3]
  print $ reverse' $ range 1 5
```

[PureScript 0.7で(x:xs)という糖衣構文が廃止された](https://github.com/purescript/purescript/wiki/0.7-Migration-Guide)ため、明示的にArrayを扱わなければならなくなりました。

## 追記

コメント欄でのkakkun61さんにArrayよりもSequenceやList使った方が良いのでは？という指摘を受けたのでこれらも書いてみました

```hs:PureScript
module Q2 where

import Prelude hiding (map, append)
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Array (range)
import qualified Data.Sequence as S
import Data.Maybe
import Data.Int
import Data.Tuple
import qualified Data.List as L

sum' :: forall a. (Ring a)=> S.Seq a -> a
sum' arr = case S.uncons arr of
  Just tpl ->
    let x = fst tpl
        xs = snd tpl in
      x + sum' xs
  Nothing -> zero

product' :: forall a. (Ring a)=> L.List a -> a
product' arr = case L.uncons arr of
  Just { head: x, tail: xs } -> x * product' xs
  Nothing -> one

main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ show $ sum' $ S.toSeq [1,2]
  print $ product' $ L.toList $ range 1 5
```

# 階乗

> 【問5】`product`を使って`fact`（階乗）を実装してください。

```hs:Haskell
fact n = product [1..n]
```

```hs:PureScript
module Q3 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Array

fact :: Int -> Int
fact n = product $ range 1 n

-- | ```purescript
-- | > Q3.main
-- | 120
-- | unit
-- | ```
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ fact 5
```

# 垂線の交点

> 【問6】点 $(p,q)$ から直線 $ax+by=c$ に下した垂線の交点を求める関数perpPointを作成してください。aとbが両方ゼロになると解なしですが、チェックせずに無視してください。

```hs:Haskell
perpPoint (p, q) (a, b, c) = (x, y)
    where
        x = (a * c + b * d) / (a * a + b * b)
        y = (b * c - a * d) / (a * a + b * b)
        d = b * p - a * q
```

```hs:PureScript
module Q4 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Tuple
import Data.Tuple.Nested

-- | usage:
-- | ```purescript
-- | >  Hoge.perpPoint (Data.Tuple.Nested.tuple3 1.0 (-1.0) 0.0) $ Data.Tuple.Nested.tuple2 0.0 2.0
-- | Tuple (1.0) (1.0)
-- | ```
perpPoint :: forall a. (Num a)=> Tuple3 a a a -> Tuple2 a a -> Tuple2 a a
perpPoint tri dou =
  tuple2 x y
  where
    a = fst $ fst tri
    b = snd $ fst tri
    c = snd tri
    p = fst dou
    q = snd dou
    d = b*p - a*q
    y = (b*c - a*d)/(a*a + b*b)
    x = (a*c + b*d)/(a*a + b*b)

main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ perpPoint (tuple3 1.0 (-1.0) 0.0) $ tuple2 0.0 2.0
```

タプルも糖衣構文ないので辛いです。

# ROT13
> 【問7】[ROT13](http://ja.wikipedia.org/wiki/ROT13)を実装してください。

```hs:Haskell
rot13ch ch
    |  'A' <= ch && ch <= 'M'
    || 'a' <= ch && ch <= 'm' = chr $ ord ch + 13
    |  'N' <= ch && ch <= 'Z'
    || 'n' <= ch && ch <= 'z' = chr $ ord ch - 13
    | otherwise = ch

rot13 ""     = ""
rot13 (x:xs) = rot13ch x : rot13 xs
```

```hs:PureScript
module Q5 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Char
import qualified Data.String as Str
import Data.Array
import Data.Maybe

rot13 :: String -> String
rot13 "" = ""
rot13 str = case Str.uncons str of
  Just { head: chr, tail: _str } -> (toString $ rot13ch chr) ++ rot13 _str
  Nothing -> ""

rot13ch :: Char -> Char
rot13ch = \chr ->
  let n = toCharCode chr in
  case n of
      _ | 65  <= n && n < 78  -> fromCharCode (n + 13)
      _ | 78  <= n && n < 90  -> fromCharCode (n - 13)
      _ | 97  <= n && n < 110 -> fromCharCode (n + 13)
      _ | 110 <= n && n < 123 -> fromCharCode (n - 13)
      _ | otherwise           -> chr

-- | > Q5.main
-- | "Uryyb, Jbeyq!"
-- | "Hello, World!"
-- | unit
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  let hello13 = rot13 "Hello, World!"
  print hello13
  print $ rot13 hello13
```


# バブルソート

> 【問8】[バブルソート](http://www.ics.kagoshima-u.ac.jp/~fuchida/edu/algorithm/sort-algorithm/bubble-sort.html)を実装してください。

```hs:Haskell
bswap [x] = [x]
bswap (x:xs)
    | x > y     = y:x:ys
    | otherwise = x:y:ys
    where
        (y:ys) = bswap xs

bsort [] = []
bsort xs = y : bsort ys
    where
        (y:ys) = bswap xs
```

```hs:PureScript
module Q6 where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.Array
import Data.Maybe

-- | bubble sort
-- | ```purescript
-- | > Hoge.bsort [5.0,1.0,3.0,2.0,4.0,0.0]
-- | [0.0,1.0,2.0,3.0,4.0,5.0]
-- | ```
bsort :: forall a. (Ord a)=> Array a -> Array a
bsort xs =
  case uncons $ bswap xs of
    Just { head: y, tail: ys } -> y : bsort ys
    Nothing -> []

-- | ```purescript
-- | Hoge.bswap [5.0,1.0,3.0,2.0,4.0,0.0]
-- | [0.0,5.0,1.0,3.0,2.0,4.0]
-- | ```
bswap :: forall a. (Ord a)=> Array a -> Array a
bswap xs = case uncons xs of
  Nothing -> xs
  Just { head: y, tail: ys } ->
    let zs = bswap ys in
    case uncons zs of
      Nothing -> xs
      Just { head: a, tail: as } ->
        if y > a
        then a:y:as
        else y:a:as

-- | ```purescript
-- | > Q6.main
-- | [1,4,3,2,5]
-- | [1,2,3,4,5]
-- | [1,2,3,4,5]
-- | [1,2,3,4,5,6,7,8,9]
-- | unit
-- | ```
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ bswap [4, 3, 1, 5, 2]
  print $ bsort [4, 3, 1, 5, 2]
  print $ bsort [5, 4, 3, 2, 1]
  print $ bsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
```

配列のパターンマッチができないのでつらい。


# マージソート
> 【問9】マージソートを実装してください。

```hs:Haskell
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take h xs)) (msort (drop h xs))
    where
        h = (length xs) `div` 2
```

```hs:PureScript
module Q7 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Data.Maybe

merge :: forall a. (Ord a)=> Array a -> Array a -> Array a
merge xs ys
  | length ys == 0 = xs
  | length xs == 0 = ys
  | otherwise      =
    case uncons xs of
      Nothing -> []
      Just {head: a, tail: as} ->
        case uncons ys of
          Nothing -> []
          Just {head: b, tail: bs} ->
            if a < b
            then a : merge as (b:bs)
            else b : merge (a:as) bs

msort :: forall a. (Ord a)=> Array a -> Array a
msort xs
  | length xs == 0 = []
  | length xs == 1 = xs
  | otherwise      = merge (msort (take h xs)) (msort (drop h xs))
  where
    h = (length xs) `div` 2

-- | ```purescript
-- | > Q7.main
-- | [1,2,3,4,5,6,7,8,9]
-- | unit
-- | ```
main:: forall a. Eff (console :: CONSOLE | a) Unit
main = do
  print $ msort [4, 6, 9, 8, 3, 5, 1, 7, 2]
```

配列のパターンマッチができないのでつらい。


# 直角三角形の三辺
> 【問11】三辺の長さが各20以下の整数で構成される直角三角形を列挙してください。並び順による重複を排除する必要はありません。

```hs:Haskell
main = do
    print [(a, b, c)
          | a <- [1..20], b <- [1..20], c <- [1..20]
          , a * a + b * b == c * c
          ]
```

```hs:PureScript
module Q9 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Array
import Data.Tuple
import Data.Tuple.Nested

-- | right triangle
-- | ```purescript
-- | > Hoge.triangle
-- | "[Tuple (Tuple (3) (4)) (5),Tuple (Tuple (4) (3)) (5),Tuple (Tuple (5) (12)) (13),Tuple (Tuple (6) (8)) (10),Tuple (Tuple (8) (6)) (10),Tuple (Tuple (8) (15)) (17),Tuple (Tuple (9) (12)) (15),Tuple (Tuple (12) (5)) (13),Tuple (Tuple (12) (9)) (15),Tuple (Tuple (12) (16)) (20),Tuple (Tuple (15) (8)) (17),Tuple (Tuple (16) (12)) (20)]"
-- | unit
-- | ```
triangle :: forall a. Eff (console :: CONSOLE | a) Unit
triangle = do
  print $ show z
  where
    x = (range 1 20)
    y = concat $
      concat $
        map (\i ->
          map (\j ->
            map (\k ->
              tuple3 i j k) x) x) x
    z = filter (\tri ->
      let a = fst $ fst tri
          b = snd $ fst tri
          c = snd $ tri in
          a*a + b*b == c*c
      ) y
```

配列内包表記がないのがつらいです。

## 追記
コメント欄にてkakkun61さんの通り[配列内包表記](http://hiruberuto.bitbucket.org/purescript/chapter08.html)についての記事があったので参考にして作ってみたら、だいぶマシになりました。なるほど

```hs:PureScript
triangle :: forall a. Eff (console :: CONSOLE | a) Unit
triangle = do
  print $ show do
    a <- range 1 20
    b <- range 1 20
    c <- range 1 20
    if a*a + b*b == c*c then return $ tuple3 a b c else []
```
# 連絡

もっと良い方法がありましたらコメントください
