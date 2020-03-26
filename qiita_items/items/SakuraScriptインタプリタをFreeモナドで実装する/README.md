# SakuraScriptインタプリタをFreeモナドで実装する

この記事は [伺か Advent Calendar 2016](http://www.adventar.org/calendars/1472)の4日目の記事です。

Free モナドを使えばインタプリタが簡単に実装できると聞いて PureScript で Aff を使いがてら簡易的な SakuraScript インタプリタを書いてみた。

Free モナドとはなんなのかについては参考リンクを参照。

## コード

```haskell
module Main where

import Prelude 

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)

import Test.Unit.Console (TESTOUTPUT, print)

import Control.Monad.Aff (Aff, forkAff, launchAff, later')
import Control.Monad.Aff.Console (log)

import Control.Monad.Free (Free, foldFree, liftF)

import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.Array (foldM)
import Data.NaturalTransformation (NaturalTransformation)



data SakuraScriptF a =
    ChangeScope Int a
  | ChangeSurface Int a
  | Talk String a
  | Wait Int a
  | YenE a

type SakuraScript a = Free SakuraScriptF a

--                           + type NaturalTransformation f g = forall a. f a -> g a
--                           |                                    + data Aff :: # ! -> * -> *
--                           |                                    |
sakuraScriptN :: forall eff. NaturalTransformation SakuraScriptF (Aff (console :: CONSOLE, testOutput :: TESTOUTPUT | eff))
sakuraScriptN (Wait n a)          = const a <$> do
                                      log ("\\_w[" <> (show n) <> "]")
                                      later' n do pure unit
--                                    |
--                                    + later' :: forall e a. Int -> Aff e a -> Aff e a
--                                    : Runs the specified asynchronous computation later, by the specified number of milliseconds.
sakuraScriptN (Talk s a)          = const a <$> do
                                      let arr = (split (Pattern "") s)
                                      foldM wait unit arr -- 1文字づつ出力
                                      log "" -- 改行
                                      where 
                                        wait _ c = later' 50 do liftEff $ print c -- 改行せず文字出力
sakuraScriptN (ChangeScope n a)   = const a <$> log ("\\p[" <> (show n) <> "]")
sakuraScriptN (ChangeSurface n a) = const a <$> log ("\\s[" <> (show n) <> "]")
--                                  |       |   |                     |
--                                  |       |   |                     + append :: a -> a -> a
--                                  |       |   + log :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
--                                  |       + map :: forall a b. (a -> b) -> f a -> f b
--                                  + const :: forall a b. a -> b -> a
--                                  : Returns its first argument and ignores its second.
sakuraScriptN (YenE a)            = const a <$> log "\\e"


run :: forall eff. NaturalTransformation SakuraScript (Aff (console :: CONSOLE, testOutput :: TESTOUTPUT | eff))
run = foldFree sakuraScriptN
--    |
--    + foldFree :: forall f m. MonadRec m => (f ~> m) -> (Free f) ~> m

onBoot :: SakuraScript Unit
onBoot = do
  liftF (ChangeScope 1 unit)
  liftF (ChangeSurface 10 unit)
  liftF (Talk "なんや、えらい難しい言語やな。" unit)
  liftF (Wait 1000 unit)
  liftF (ChangeScope 0 unit)
  liftF (ChangeSurface 0 unit)
  liftF (Talk "そうだねー。" unit)
  liftF (YenE unit)
-- |
-- + liftF :: forall f. f ~> Free f


main :: forall eff. Eff ( console :: CONSOLE , err :: EXCEPTION, testOutput :: TESTOUTPUT | eff ) Unit
main = do
  void $ launchAff do
-- |     |
-- |     + launchAff :: forall e a. Aff e a -> Eff (err :: EXCEPTION | e) (Canceler e)
-- |     : Converts the asynchronous computation into a synchronous one. All values are ignored, and if the computation produces an error, it is thrown.
-- + void :: forall f a. Functor f => f a -> f Unit
    forkAff do
      run $ onBoot
--  |
--  + forkAff :: forall e a. Aff e a -> Aff e (Canceler e)
--  : Forks the specified asynchronous computation so subsequent computations will not block on the result of the computation.
--  : Returns a canceler that can be used to attempt cancellation of the forked computation.

```

## 結果

```shell-session
$ psc --version
0.10.2
$ pulp run
* Building project in ***/purs-free
Compiling Main
* Build successful.
\p[1]
\s[10]
なんや、えらい難しい言語やな。
\_w[1000]
\p[0]
\s[0]
そうだねー。
\e

```


## 小学生並みの感想

* 文字列結合は `<>` で Semigroup にある
* `~>` とは一体何なのか？(Naturalで、自然変換です。型オペレータも定義できるので
* `YenE` で処理を打ち切りたかったが方法が分からなかった。 Nothing とか使えばいいのだろうか？
* `∷` や `→` や `∀` が使えるようになったようだ？
* 改行なし出力のために `Test.Unit.Console` を使った


## 参考
* http://d.hatena.ne.jp/fumiexcel/20121111/1352614885
* http://d.hatena.ne.jp/its_out_of_tune/comment/20121111/1352632815
* https://github.com/purescript/purescript-free
* http://ssp.shillest.net/ukadoc/manual/list_sakura_script.html
