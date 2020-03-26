あとから機能を追加できる

```hs
import Prelude
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect.Aff (Aff, launchAff_, delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main = do
  launchAff_ do
    changeScope 0
    changeSurface 0
    liftEffect $ changeBalloonSurface 0
    talk "こんにちは。"
    wait 1000
    changeScope 1
    liftEffect $ changeBalloonSurface 2
    changeSurface 10
    talk "こんにちは。"
    wait 1000
    sakura
    talk "さくらです"
    wait 1000
    unyu
    talk "うにゅうです"
    wait 1000
    yenE
  pure unit

class SakuraScriptV1Sym f where
  changeScope :: Int -> f Unit
  changeSurface :: Int -> f Unit
  talk :: String -> f Unit
  wait :: Int -> f Unit
  yenE :: f Unit

instance ssV1Sym :: SakuraScriptV1Sym Aff where
  changeScope x = "\\p[" <> (show x) <> "]" # log >>> liftEffect
  changeSurface x = "\\s[" <> (show x) <> "]" # log >>> liftEffect
  talk = log >>> liftEffect
  wait = toNumber >>> Milliseconds >>> delay
  yenE = "\\e" # log >>> liftEffect


class SakuraScriptV2Sym f where
  changeBalloonSurface :: Int -> f Unit

instance ssV2Sym :: SakuraScriptV2Sym Effect where
  changeBalloonSurface x = "\\b[" <> (show x) <> "]" # log

class (SakuraScriptV1Sym f) <= SakuraScriptV3Sym f where
  sakura :: f Unit
  unyu :: f Unit

instance ssV3Sym :: SakuraScriptV3Sym Aff where
  sakura = changeScope 0
  unyu = changeScope 1
```
