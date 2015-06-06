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
            [ ("position", "absolute")
            , background Color.charcoal
            , width 10
            , height 10
            , ("margin-left", (toString (x-5)) ++ "px")
            , ("margin-top", (toString (y-5)) ++ "px")
            ]
    in
        Html.div [Html.style style] []

circle (x,y) f =
    let
        w = (round (300*f))
        h = (round (300*f))
        style =
            [ ("position", "absolute")
            , background Color.blue
            , ("opacity", toString (1-f))
            , width w
            , height h
            , ("margin-left", (toString (x-w//2)) ++ "px")
            , ("margin-top", (toString (y-h//2)) ++ "px")
            ]
    in
        Html.div [Html.style style] []
render time model =
    let
        ease = animateIntTuple Easing.easeInOutQuad time model.location
        easeClick = animateValue Easing.float Easing.easeInQuad time model.click
    in
        Html.div [Html.style [("position", "relative")]]
            [ box (ease |> fst) (ease |> snd)
            , circle model.clickLocation easeClick
            ]

init = 
    { location = animationState (40,400)
    , click = animationState 1.0
    , clickLocation = (0,0)
    }

distance (a,b) (x,y) = sqrt (toFloat <| (a-x)*(a-x) + (b-y)*(b-y))

step (time,(x,y)) model =
    let
        lastLocation = currentTargetValue model.location
        d = distance (x,y) lastLocation
        duration = sqrt (d*4000)
    in
    { model
    | location <- startAnimation duration time (x,y) model.location
    , click <- model.click |> clearAnimation 0.0 |> startAnimation 350 time 1.0
    , clickLocation <- (x,y)
    }

main = animationSignal init step render (Signal.sampleOn Mouse.clicks Mouse.position)
