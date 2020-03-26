Aff に代えた

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
    talk "こんにちは。"
    wait 1000
    changeScope 1
    changeSurface 10
    talk "こんにちは。"
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
```
