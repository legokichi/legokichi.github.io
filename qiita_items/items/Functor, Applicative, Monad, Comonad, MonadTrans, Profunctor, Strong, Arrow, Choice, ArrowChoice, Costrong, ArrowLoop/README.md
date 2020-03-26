## Functor, Applicative, Monad, Comonad

![monad.jpg](https://qiita-image-store.s3.amazonaws.com/0/13484/f643b6ae-115a-fb93-be85-4517265f4620.jpeg)

## Kleisli 圏の射 `>=>`

![kleisli.jpg](../../img/7_g.jpeg)

## MonadTrans

![trans.jpg](https://qiita-image-store.s3.amazonaws.com/0/13484/75ffcd6e-998c-129a-1bb8-c98092fa4cbc.jpeg)

## Profunctor, Strong, Arrow

![arrow.jpg](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/13484/25ba614b-e562-c146-9c70-211dd6399235.jpeg)

## Arrow, ArrowLoop

![arrow2.jpg](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/13484/9e74e952-0e9d-a6f4-672b-567796fa3a46.jpeg)

## Profunctor, Strong, Arrow, Choice, ArrowChoice, Costrong, ArrowLoop

PureScript による Strong, Costrong, Choice のデモコード
PureScript は正格評価のため遅延評価が必要な Costrong (->) の loop == unfirst を実装していない
個々では型だけ合わせて擬似的に再現したが unfirst, unsecond は本来の意図通りのフィードバックループとしては機能しない

```hs
-- purescript 0.13.3

import Control.Applicative (pure)
import Control.Bind ((>>=), discard, bind)
import Control.Semigroupoid ((>>>))
import Data.Eq (class Eq, (==))
import Data.Either (Either(Left, Right))
import Data.Function (($), (#))
import Data.Profunctor (class Profunctor dimap, arr)
import Data.Profunctor.Choice ((+++), (|||))
import Data.Profunctor.Costrong (class Costrong, unfirst unsecond)
import Data.Profunctor.Strong ((&&&), (***), first, second)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Arrow Demo"
  log $ ("arrow: " <> _) $ show $ A # a2bc >>> bc2cc >>> cc2aa >>> (second a2b)
  log $ ("choice: " <> _) $ show $ Left A # (a2b +++ a2c) >>> (b2c +++ c2a) >>> (c2a +++ a2b) >>> (a2c ||| b2c)
  log $ ("loop: " <> _) $ show $ let Function' a2a = unfirst (Function' ooo) in a2a $ a2a A
  log $ ("loop2: " <> _) $ show $ let Function' f = unfirst (Function' iii) in f true
  pure unit
  where
  ooo :: (Tuple A (Maybe Int)) -> (Tuple A (Maybe Int))
  ooo = case _ of
    Tuple a (Just c) ->
      Tuple a $ Just $ spy "ooo" $ c + 1
    Tuple a _ -> Tuple a $ Just $ spy "ooo" 0

  iii :: (Tuple Boolean (Unit -> Int)) -> (Tuple Boolean (Unit -> Int))
  iii (Tuple reset lazy) = case reset of
    true -> Tuple false \_ -> spy "iii" 0
    false -> Tuple false $ \_ -> spy "iii" $ 1 + lazy unit

  a2b :: A -> B
  a2b A = B

  a2c:: A -> C
  a2c A = C

  b2c :: B -> C
  b2c B = C

  c2a :: C -> A
  c2a C = A

  a2bc :: A -> (Tuple B C)
  a2bc = (a2b &&& a2c)

  bc2cc :: (Tuple B C) -> (Tuple C C)
  bc2cc = first b2c

  cc2aa :: (Tuple C A) -> (Tuple A A)
  cc2aa = (c2a *** c2a)

--- Costrong (->) === ArrowLoop のために必要
--- PureScript は正格評価のため遅延評価が必要な Costrong (->) を実装できない!!! 

newtype Function' a b = Function' (a -> b)

instance profunctorFn' :: Profunctor Function' where
  dimap a2b c2d (Function' b2c) = Function' (a2b >>> b2c >>> c2d)

instance costrongFn' :: Costrong Function' where
  unfirst :: forall a b c. (Function' (Tuple a c) (Tuple b c)) -> Function' a b
  unfirst (Function' f) =
    Function' \a ->
      let
        (Tuple b c) = f (Tuple a c)
      in
        b
    where
    c = undefined -- 正格評価の PureScript ではフィードバック値 c の初期値を遅延評価で算出できない!!!
  unsecond :: forall a b c. (Function' (Tuple a b) (Tuple a c)) -> Function' b c
  unsecond (Function' f) =
    Function' \b ->
      let
        (Tuple a c) = f (Tuple a b)
      in
        c
    where
    a = undefined -- 正格評価の PureScript ではフィードバック値 a の初期値を遅延評価で算出できない!!!

data A = A
derive instance eqA :: Eq A
instance showA :: Show A where
  show A = "A"

data B = B
derive instance eqB :: Eq B
instance showB :: Show B where
  show B = "B"

data C = C
derive instance eqC :: Eq C
instance showC :: Show C where
  show C = "C"

data D = D
derive instance eqD :: Eq D
instance showD :: Show D where
  show D = "D"
```


## Kleisli Triple

todo...


## referencce

* Control.Monad - https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Control.Monad
* Control.Comonad - https://pursuit.purescript.org/packages/purescript-control/3.3.0/docs/Control.Comonad
* Arrowを理解する - https://qiita.com/Lugendre/items/6b4a8c8a9c85fcdcb292
* Arrows are Strong Monads - http://www-kb.is.s.u-tokyo.ac.jp/~asada/papers/arrStrMnd.pdf
* What's the relationship between profunctors and arrows? - https://stackoverflow.com/questions/38169453/whats-the-relationship-between-profunctors-and-arrows


## reference2
 
* Arrowまわりの射の合成いろいろ - https://qiita.com/yasuabe2613/items/11a04913ae5cf9dfa7ee
* じゃあ(->)以外のArrow使うことほぼないから(->)だけで考えればいいんじゃない？(投げやりな態度) - https://mobile.twitter.com/myuon_myon/status/912681322610540544
* <blockquote class="twitter-tweet"><p lang="en" dir="ltr">-&gt; has the same kind as Either<br><br> (-&gt;) :: Type -&gt; Type -&gt; Type<br> Either :: Type -&gt; Type -&gt; Type<br><br>but they can&#39;t get assigned the same arrows (-&gt;) (-&gt;) (-&gt;), we cannot define<br><br> bimap :: (a-&gt;a&#39;) -&gt; (b-&gt;b&#39;) -&gt; ((a-&gt;b)-&gt;(a&#39;-&gt;b&#39;))<br><br>because functions are wIeRd</p>&mdash; _j (@Iceland_jack) <a href="https://twitter.com/Iceland_jack/status/1129746235697586177?ref_src=twsrc%5Etfw">May 18, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
* <blockquote class="twitter-tweet"><p lang="en" dir="ltr">Data.Bifunctor.bimap @​Either<br>:: (a -&gt; a&#39;)<br>-&gt; (b -&gt; b&#39;)<br>-&gt; (Either a b -&gt; Either a&#39; b&#39;)<br><br>Data.Profunctor.dimap @(-&gt;)<br>:: (a &lt;- a&#39;)<br>-&gt; (b -&gt; b&#39;)<br>-&gt; ((a -&gt; b) -&gt; (a&#39; -&gt; b&#39;))</p>&mdash; _j (@Iceland_jack) <a href="https://twitter.com/Iceland_jack/status/1129746954651623424?ref_src=twsrc%5Etfw">May 18, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
* <blockquote class="twitter-tweet"><p lang="en" dir="ltr">For each configuration of arrows, we need a new friggin type class. This is holding back Haskell the most (my opinion, you can&#39;t say anything!)<br><br> Functor: (-&gt;) (-&gt;)<br> Contravariant: (&lt;-) (-&gt;)<br> Bifunctor: (-&gt;)(-&gt;)(-&gt;)<br> Profunctor: (&lt;-)(-&gt;)(-&gt;)<br><br>this is not so complicated</p>&mdash; _j (@Iceland_jack) <a href="https://twitter.com/Iceland_jack/status/1129747766056488961?ref_src=twsrc%5Etfw">May 18, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
* Arrowまわりの射の合成いろいろ - https://qiita.com/yasuabe2613/items/11a04913ae5cf9dfa7ee
* Promonads, Arrows, and Einstein Notation for Profunctors - https://bartoszmilewski.com/2019/03/27/promonads-arrows-and-einstein-notation-for-profunctors/
* Profunctorを咀嚼する - https://its-out-of-tune.hatenadiary.org/entry/20130407/1365350952
* https://github.com/shiatsumat/wiwinwlh-jp/wiki/アプリカティブ#アロー
* 関手ー双関手- http://bitterharvest.hatenablog.com/entry/2017/02/23/071611
* Prismメモ - https://debug-ito.hatenablog.com/entry/20150112/1421028231
* Arrows-based Functional Reactive Programming - https://wiki.haskell.org/Arrows-based_Functional_Reactive_Programming
* Yampa - https://wiki.haskell.org/Yampa
* Arrow tutorial - https://wiki.haskell.org/Arrow_tutorial
* ArrowによるHaskellプログラミングの基礎。…パイプ感覚で順次/分岐/繰返し - https://r-west.hatenablog.com/entry/20070720/1184946510
* Haskell/Arrows - https://ja.wikibooks.org/wiki/Haskell/Arrows
* Visually Programming Arrows and Arrowized Functional Reactive Programming - https://imve.informatik.uni-hamburg.de/projects/VisualAFRP
* Arrows: A General Interface to Computation - https://www.haskell.org/arrows/
* Haskell/Understanding arrows - https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
* The ArrowApply class is equivalent to Monad: any monad gives rise to a Kleisli arrow, and any instance of ArrowApply defines a monad. - https://twitter.com/paf31/status/983017352533962753
* MonadとArrowの関係。 - https://r-west.hatenablog.com/entry/20070531/1180630841
* The Arrow class is redundant #9 - https://github.com/purescript-deprecated/purescript-arrows/issues/9
    * An Arrow isn't a Strong Category with extra stuff, it is precisely Strong + Category.
* Add Loop #2 - https://github.com/purescript/purescript-profunctor/issues/2
    * Does ArrowLoop depend on Haskell's lazy evaluation? It seems that Halogen's loop relies on an initial state.
    * Does Costrong cover this? unfirst is loop anyway, but there's also unsecond.
* https://pursuit.purescript.org/packages/purescript-profunctor/4.1.0/docs/Data.Profunctor
* https://pursuit.purescript.org/packages/purescript-profunctor/4.1.0/docs/Data.Profunctor.Strong
* `class ArrowLoop a => ArrowCircuit a where` https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Operations.html#g:6
* haskell – 右矢印ArrowLoopの法則 - https://codeday.me/jp/qa/20190414/625979.html
