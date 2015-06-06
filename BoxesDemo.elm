import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Easing
import Color
import Time exposing (Time)
import String
import Debug
import Animation.Last exposing (..)
        
-- --
-- -- DEMO APP
-- --

box time model delay =
    let
        ease = animateOnOff Easing.easeInOutQuad (time-delay) model
        size = ease easeInt 100 200
    in
        [ Html.div
            [ Html.style
                [ width size, height size
                , background <| ease Easing.color Color.red Color.blue
                , transform [rotate <| ease Easing.float 0 180]]
            , Html.onMouseEnter tbox.address (Animate True)
            , Html.onMouseLeave tbox.address (Animate False)
            ] []
        ] |> Html.div [Html.style [width 200, height 200, background Color.lightGrey, ("display", "inline-block")]]

render time model =
    [1..8]
    |> List.map ((*) 50 >> box time model)
    |> Html.div []
    |> \html -> Html.div []
        [ html
        , Html.text (toString time), Html.br [] []
        , Html.text (toString model)
        ]

tbox = Signal.mailbox Init

init = animationState False

type Action = Animate Bool | Init
type alias Model = AnimationState Bool

step : (Time,Action) -> Model -> Model
step (time,action) model = case action of
    Init -> model
    Animate value -> startAnimation Time.second time value model

main = animationSignal init step render tbox.signal
