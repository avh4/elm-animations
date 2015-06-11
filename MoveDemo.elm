import Html exposing (Html)
import Html.Attributes as Html
import Time exposing (Time)
import Animation.Last exposing (..)
import Color exposing (Color)
import Easing
import Mouse

box : (Int, Int) -> Html
box (x,y) =
    let
        style =
            [ background Color.charcoal
            , width 10, height 10
            , ("margin-left", (toString x) ++ "px")
            , ("margin-top", (toString y) ++ "px")
            ]
    in
        Html.div [Html.style style] []

type alias Model = AnimationState (Int,Int)
type alias Action = (Int,Int)

render : Time -> Model-> Html
render time model = box (currentValue time model)

init : Model
init = animationState (pair int) (40,400)

step : (Time,Action) -> Model -> Model
step (time,(x,y)) model =
    startAnimation Easing.easeInOutQuad Time.second 0 time (x,y) model

actions : Signal Action
actions = Signal.sampleOn Mouse.clicks Mouse.position

main : Signal Html
main = animationSignal init step render isActive actions
