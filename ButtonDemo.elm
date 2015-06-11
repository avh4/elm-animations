import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Time exposing (Time)
import Animation.Last exposing (..)
import Color exposing (Color)
import Easing
--import Controls exposing (..)
import MaterialDesign.Button as Button

--hoverSlider = floatSlider "hover" 0 1.0 0.1 0
--clickSlider = floatSlider "click" 0 1.0 0.1 0
--xSlider = intSlider "x" 0 100 1 60
--ySlider = intSlider "y" 0 100 1 20

buttonA = Signal.mailbox <| Button.Hover False
buttonB = Signal.mailbox <| Button.Hover False

render : Time -> Model -> Html
render time model =
    Html.div [Html.style [("padding", "8px")]]
        [ Html.p [] [Button.render (Signal.message buttonA.address) time model.a]
        , Html.p [] [Button.render (Signal.message buttonB.address) time model.b]
        --, hoverSlider.html
        --, clickSlider.html
        --, xSlider.html
        --, ySlider.html
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
    --Hover f -> { m | hover <- f }
    --Click f -> { m | click <- f }
    --X i -> { m | clickLocation <- (i, snd m.clickLocation) }
    --Y i -> { m | clickLocation <- (fst m.clickLocation, i) }
    --case (currentOnOffTargetValue model) of
    --True -> startOnOffAnimation Easing.easeInOutQuad 700 0 time False model
    --False -> startOnOffAnimation Easing.easeInOutQuad 700 0 time True model

type Action
    = Hover Float
    | Click Float
    | X Int | Y Int
    | ButtonA Button.Action
    | ButtonB Button.Action

actions = Signal.mergeMany
    --[ Signal.map Hover hoverSlider.signal
    --, Signal.map Click clickSlider.signal
    --, Signal.map X xSlider.signal
    --, Signal.map Y ySlider.signal
    [ Signal.map ButtonA buttonA.signal
    , Signal.map ButtonB buttonB.signal
    ]

isActive t m = Button.isActive t m.a || Button.isActive t m.b

main = animationSignal init step render isActive actions
