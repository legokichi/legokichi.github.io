Elm を初めて触ってみたのですが、 Video 要素などの扱いはどうなんだろうと、 [slack](https://elmlang.slack.com) で訊いてみたところ 「[custom dom event handler を作れ](https://robots.thoughtbot.com/building-custom-dom-event-handlers-in-elm)」と言われたので実装してみました。

![](https://i.gyazo.com/56a0e683cbd110699361f497d010ae65.png)

http://elm-lang.org/try にコピペすると現在シーク時刻とビデオ再生状態が取得できます。

```elm
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = { videoState : String
                   , currentTime : Float }

init : (Model, Cmd Msg)
init = (Model "loading" 0, Cmd.none)

-- UPDATE

type Msg = CurrentTime Float
         | Loading
         | Playing
         | Paused
         | Seeking

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CurrentTime currentTime ->
      ({ model | currentTime = currentTime }, Cmd.none)
    Loading ->
      ({ model | videoState = "loading" }, Cmd.none)
    Paused ->
      ({ model | videoState = "paused" }, Cmd.none)
    Playing ->
      ({ model | videoState = "playing" }, Cmd.none)
    Seeking ->
      ({ model | videoState = "seeking" }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

targetCurrentTime : Json.Decoder Float
targetCurrentTime = Json.at ["target", "currentTime"] Json.float

view : Model -> Html Msg
view model =
  div [] [
    video [ src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
          , on "timeupdate" (Json.map CurrentTime targetCurrentTime)
          , on "seek" (Json.map CurrentTime targetCurrentTime)
          , on "seek" (Json.succeed Seeking)
          , on "seeking" (Json.succeed Seeking)
          , on "seekend" (Json.succeed Paused)
          , on "playing" (Json.succeed Playing)
          , on "play" (Json.succeed Playing)
          , on "pause" (Json.succeed Playing)
          , on "ended" (Json.succeed Paused)
          , on "loadedmetadata" (Json.succeed Paused)
          , controls True] [],
    div [] [text (toString model.currentTime)],
    div [] [text (toString model.videoState)]
  ]
```

## 所管

Elm は入出力が増えたら Msg が大変なことになりそうだと思いました（小学生並の感想

## 参考

* https://robots.thoughtbot.com/building-custom-dom-event-handlers-in-elm
* https://github.com/elm-lang/html/blob/2.0.0/src/Html/Events.elm#L251
* http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Events
* http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode
