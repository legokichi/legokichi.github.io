# Applicative スタイル `f <$> m1 <*> m2` を読み解く
Applicative スタイルとは

```hs
foo = do
  a <- m1
  b <- m2
  return $ f a b
```

という計算に対し、 do と return を消して

```hs
foo = f <$> m1
        <*> m2
```

という風に書けるよという話です。

問題はこの `f <$> m1 <*> m2` では実際に何が起きているのか、なぜ同じ計算結果になるのか、についてです。

## `<$>` と `<*>` は `fmap` と `apply`

`<$>` と `<*>` という見慣れた、しかし初心者には分からん殺しの演算子ですが、両方とも `infixl 4` なので、普通の左から順に計算できる2項演算です。

### `<$>` は `fmap`
まずこの `<$>` についてですが、 `fmap` という Functor 型クラスに関する関数です。様々な言語でリストに対する map としてよく使われているアレです。

```hs
fmap :: Functor f => (a -> b) -> f a -> f b
```
この型は一見すると 2項演算 のように見えますが、 Haskell では全ての関数はカリー化されているので

```hs
fmap :: Functor f => (a -> b) -> (f a -> f b)
```
という結合順序で読むと、関数 `(a -> b)` を引数に取り関数 `f a -> f b` を返す1項演算子と見ることができます。まさしく Functor ですね。
### `<*>` は `apply`
次に `<*>` についてですが、これは Applicative 型クラスに関する関数 `apply` です。
`([\x->x+1, \x->x+2, \x->x+3] <*> [1, 2, 3]) == [2,3,4, 3,4,5, 4,5,6]` というように使えます。

```hs
apply :: Applicative f => f (a -> b) -> f a -> f b
```
例によって1項演算子として見ると

```hs
apply :: Applicative f => f (a -> b) -> (f a -> f b)
```

と読めます。

## Applicative スタイル を読み解く

では改めて

```hs
foo = f <$> m1 <*> m2
```

を式変形していきましょう。

```hs
foo = f <$> m1 <*> m2
foo = (f <$> m1) <*> m2
foo = (f `fmap` m1) <*> m2
foo = ((fmap f) m1) <*> m2
foo = ((fmap f) m1) `apply` m2
foo = (apply ((fmap f) m1)) m2
foo = ((apply ((fmap f) m1)) m2)
```

`((apply ((fmap f) m1)) m2)` という形になり、これで計算の順序は一点の曇りなく書くことができました。
全てのカッコでの型を書くと。

```hs
--      + apply :: Applicative f => f (a -> b) -> (f a -> f b)
--      |       + fmap :: Functor f => (a -> b) -> (f a -> f b)
--      |       |    + a -> b -> c
--      |       |    |  + f a
--      |       |    |  |    + f b
--      |       |    |  |    |
foo = ((apply ((fmap f) m1)) m2)
--    ||      ||
--    ||      |+ f a -> f (b -> c)
--    ||      + f (b -> c)
--    |+ f b -> f c
--    + f c
```
となっています。というわけで `foo` の型は `Applicative f => f c` でした

## do スタイルとの比較
なぜこの Applicative スタイル が do スタイルの結果と一致するのでしょうか。

この計算

```hs
foo = do
  a <- m1
  b <- m2
  return (f a b)
```

でやりたかったのは、ある Monad `m1` と `m2` の中身を取り出して関数 `f` に適用し、その結果 `f a b` を何らかの Monad で包むことです。 

ところで、 Monad とは Applicative な Functor (Monad ⊆ Applicative ⊆ Functor) のうち2項演算子 `bind` (`>>=`) という性質もつもののことです。この `>>=` があることによって計算の結果を見て条件分岐し,
結果を変えることができます。

Monad 的な if 3項演算子 `miffy` を考えてみます。

```hs
miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt me = do
  b <- mb
  if b then mt else me
{-以下のような意味です
  if b
    then do
      ret <- mt
      return ret
    else do
      ret <- me
      return ret
-}
```

mb の実行結果によって2つの mt と me からどちらか1つを実行し、結果を返す関数です。

Applicative Functor についても考えてみます。

```hs
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = (pure cond) <*> fb <*> ft <*> fe
  where
    cond b t e = if b then t else e
```

しかしこの定義には問題があります。

```hs
iffy (pure True) (pure t) Nothing == Nothing
```

条件は `True` なので結果は　`t` になるべきですが `Nothing` を返してしまいます。なぜなら、

```hs
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing
```

の通り、MaybeのFunctor型クラスのインスタンスはfmapの第二引数にNothingがくるとNothingを返すように定義されているからです。

Monad 版では問題ありません。

```hs
miffy (return True) (return t) Nothing == (return t)
```

このことから、Monad は Applicative Functor のうち条件分岐が可能な性質を持つものと見ることができます。

件の計算

```hs
foo = do
  a <- m1
  b <- m2
  return (f a b)
```

では m1の実行結果を参照して次に実行するモナドをm2とは別のモナドに分岐するような処理はありませんでした。
そのため、計算 foo はモナドのうち Applicative の性質のみで記述することができたのです。

逆に言えば、条件分岐する必要がなければ IO モナドであっても Applicative スタイル で逐次実行を書くことができます。

```hs
main :: IO ()
main = id <$> putStrLn "hello world"

main :: IO ()
main = (const id) <$> putStrLn "hello"
                  <*> putStrLn "world"

main :: IO ()
main = (const (const id)) <$> putStrLn "hello"
                          <*> putStrLn "real"
                          <*> putStrLn "world"
```

面白いですね。

## 参考
* http://www.staff.city.ac.uk/~ross/papers/Applicative.html
* http://d.hatena.ne.jp/kazu-yamamoto/20101211/1292021817
* http://qiita.com/lex_naturalis/items/692701334d8b9650c26d
* http://qiita.com/tacke_jp/items/bc55d1313ff572e6661e

