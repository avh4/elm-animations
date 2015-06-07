import Html exposing (Html)
import Html.Attributes as Html
import Time exposing (Time)
import Animation.Last exposing (..)
import Color exposing (Color)
import Easing
import Mouse

box : Int -> Int -> Html
box x y =
    let
        style =
            [ background Color.charcoal
            , width 10
            , height 10
            , ("margin-left", (toString x) ++ "px")
            , ("margin-top", (toString y) ++ "px")
            ]
    in
        Html.div [Html.style style] []

render time model =
    let
        ease = currentValue time model
    in
        Html.div []
            [ box (ease |> fst) (ease |> snd)
            ]

intPairAnimationState =
    let
        addPair (a,b) (x,y) = (a+x,b+y)
        diffPair (a,b) (x,y) = (a-x,b-y)
        zeroPair = (0,0)
    in
        animationState (Easing.pair easeInt) zeroPair addPair diffPair

init = intPairAnimationState (40,400)

step (time,(x,y)) model =
    startAnimation Easing.easeInOutQuad Time.second time (x,y) model

main = animationSignal init step render (isActive) (Signal.sampleOn Mouse.clicks Mouse.position)
