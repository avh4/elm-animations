import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Time exposing (Time)
import Animation exposing (..)
import Color exposing (Color)
import Easing
import MaterialDesign.Button as Button

buttons = Signal.mailbox <| Button 0 <| Button.Hover False

render : Time -> Model -> Html
render time model =
    model
    |> List.indexedMap (\i (a,m) -> Html.p [
        Html.style [("transform", "translate(0," ++ (toString (10*(1-(currentValue time a)))) ++ "px)")
        , ("opacity",toString (currentValue time a)) ]
        ] [Button.render (Button i >> Signal.message buttons.address) time m])
    |> Html.div [Html.style [("padding", "8px")]]

type alias Model = List (AnimationState Float, Button.Model)

init : Model
init = [ (animationState float 1.0, Button.init Color.green "Add") ]

step : (Time, Action) -> Model -> Model
step (time,a) m = case a of
    Button i (Button.Click c) ->
       (animationState float 0.0 |> startAnimation Easing.easeOutQuad 400 0 time 1.0, (Button.init Color.red (toString i)))
       :: (List.reverse <| List.indexedMap (\i' (a,m) -> if i' == i then (a,Button.step (time,Button.Click c) m) else (a,m) ) m)
       |> List.reverse
    Button i a' ->
       List.indexedMap (\i' (a,m) -> if i' == i then (a,Button.step (time,a') m) else (a,m) ) m

type Action
    = Button Int Button.Action

actions = Signal.mergeMany
    [ buttons.signal
    ]

isActive t m = List.any (\(a,m) -> Animation.isActive t a || Button.isActive t m) m

main = animationSignal init step render isActive actions
