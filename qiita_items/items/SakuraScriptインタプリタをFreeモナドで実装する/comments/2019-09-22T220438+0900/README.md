Free Monad ではなく Tagless Final を使ったら秒で書けてやばい


```hs
import Prelude
import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Console (log)
import Data.Typelevel.Undefined (undefined)

main :: Effect Unit
main = do
  changeScope 0
  changeSurface 0
  talk "こんにちは。"
  changeScope 1
  changeSurface 10
  talk "こんにちは。"
  yenE
  pure unit

class SakuraScriptV1Sym f where
  changeScope :: Int -> f Unit
  changeSurface :: Int -> f Unit
  talk :: String -> f Unit
  wait :: Int -> f Unit
  yenE :: f Unit

instance ssV1Sym :: SakuraScriptV1Sym Effect where
  changeScope _ = pure unit
  changeSurface _ = pure unit
  talk = log
  wait _ = pure unit
  yenE = pure unit
```
