import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Time exposing (Time)
import Animation exposing (..)
import Color exposing (Color)
import Easing
import MaterialDesign.Button as Button

buttonA = Signal.mailbox <| Button.Hover False
buttonB = Signal.mailbox <| Button.Hover False

render : Time -> Model -> Html
render time model =
    Html.div [Html.style [("padding", "8px")]]
        [ Html.p [] [Button.render (Signal.message buttonA.address) time model.a]
        , Html.p [] [Button.render (Signal.message buttonB.address) time model.b]
        ]

type alias Model = { a : Button.Model, b : Button.Model }

init : Model
init =
    { a = Button.init Color.orange "Agree"
    , b = Button.init Color.purple "Cancel"
    }

step : (Time, Action) -> Model -> Model
step (time,a) m = case a of
    ButtonA a' -> { m | a <- Button.step (time,a') m.a }
    ButtonB a' -> { m | b <- Button.step (time,a') m.b }

type Action
    = ButtonA Button.Action
    | ButtonB Button.Action

actions = Signal.mergeMany
    [ Signal.map ButtonA buttonA.signal
    , Signal.map ButtonB buttonB.signal
    ]

isActive t m = Button.isActive t m.a || Button.isActive t m.b

main = animationSignal init step render isActive actions
