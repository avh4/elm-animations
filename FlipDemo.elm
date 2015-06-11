import Html exposing (Html)
import Html.Attributes as Html
import Time exposing (Time)
import Animation exposing (..)
import Color exposing (Color)
import Easing

yellowCircle : Float -> Html
yellowCircle alpha =
    Html.img
    [ Html.src "yellowCircle.png"
    , Html.style [("opacity", toString alpha)]
    ] []

box : Int -> Color -> Html
box size color =
    let
        style =
            [ background color
            , width size
            , height size
            ]
    in
        Html.div [Html.style style] []

render time model =
    let
        ease = animateOnOff time model
    in
        Html.div []
            [ box (ease easeInt 250 40) (ease Easing.color Color.blue Color.red)
            , yellowCircle (ease Easing.float 0 1)
            ]

init = onOffAnimationState True

step (time,a) model = case (currentOnOffTargetValue model) of
    True -> startOnOffAnimation Easing.easeInOutQuad 700 0 time False model
    False -> startOnOffAnimation Easing.easeInOutQuad 700 0 time True model

main = animationSignal init step render (isActive) (Time.every 1000)
