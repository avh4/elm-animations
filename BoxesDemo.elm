import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Easing
import Color exposing (Color, toRgb, rgba)
import Time exposing (Time)
import String
import Debug
import Animation.Last exposing (..)

-- --
-- -- DEMO APP
-- --

-- https://github.com/Dandandan/Easing/pull/9
colorEase : Easing.Interpolation Color
colorEase from to v =
    let
        float = Easing.float
        (rgb1, rgb2)     = (toRgb from, toRgb to)
        (r1, g1, b1, a1) = (rgb1.red, rgb1.green, rgb1.blue, rgb1.alpha)
        (r2, g2, b2, a2) = (rgb2.red, rgb2.green, rgb2.blue, rgb2.alpha)
        float' from to v = round (float (toFloat from) (toFloat to) v) |> max 0 |> min 255
    in rgba (float' r1 r2 v) (float' g1 g2 v) (float' b1 b2 v) (float a1 a2 v)

box time (i,model) =
    let
        ease = animateOnOff time model
        size = ease easeInt 100 200
    in
        [ Html.div
            [ Html.style
                [ width size, height size
                , background <| ease colorEase Color.red Color.blue
                , transform [rotate <| ease Easing.float 0 180]]
            , Html.onMouseEnter tbox.address (Animate i True)
            , Html.onMouseLeave tbox.address (Animate i False)
            ] []
        ] |> Html.div [Html.style [width 200, height 200, background Color.lightGrey, ("display", "inline-block")]]

render time model =
    model
    |> List.map (box time)
    |> Html.div []
    |> \html -> Html.div []
        [ html
        , Html.text (toString time), Html.br [] []
        , Html.text (toString model)
        ]

tbox = Signal.mailbox Init

init : Model
init =
    [0..7]
    |> List.map (\i -> (i,onOffAnimationState False))

type Action = Animate Int Bool | Init
type alias Model = List (Int,AnimationState Float)

step : (Time,Action) -> Model -> Model
step (time,action) model = case action of
    Init -> model
    Animate focus value ->
        let
            anim = startOnOffAnimation Easing.easeInOutQuad Time.second
            delay i = abs (focus-i) |> toFloat |> (*) 100
            start (i,m) = (i,anim (delay i) time value m)
        in
            model |> List.map start

main = animationSignal init step render (\t -> List.any (snd >> isActive t)) tbox.signal
