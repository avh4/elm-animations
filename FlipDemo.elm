import Html exposing (Html)
import Html.Attributes as Html
import Time exposing (Time)
import Animation.Last exposing (..)
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
        ease = animateOnOff 0 Easing.easeInOutQuad time model
    in
        Html.div []
            [ box (ease easeInt 250 40) (ease Easing.color Color.blue Color.red)
            , yellowCircle (ease Easing.float 0 1)
            ]

init = animationState True

step (time,a) model = case (currentValue model) of
    True -> startAnimation Time.second time False model
    False -> startAnimation Time.second time True model

main = animationSignal init step render (Time.every 1000)
