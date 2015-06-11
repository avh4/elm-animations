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
import Animation exposing (..)

-- --
-- -- DEMO APP
-- --

box time (i,model) =
    let
        ease = animateOnOff time model
        size = ease easeInt 100 200
    in
        [ Html.div
            [ Html.style
                [ width size, height size
                , background <| ease Easing.color Color.red Color.blue
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
