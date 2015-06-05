module Controls where

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Result
import Maybe
import String
import Signal

slider : String -> a -> a -> a -> a -> (String -> Signal.Message) -> Html
slider name min max step default action =
    Html.label []
        [ Html.text name
        , Html.input
            [ Html.type' "range"
            , Html.min <| toString min
            , Html.max <| toString max
            , Html.step <| toString step
            , Html.attribute "value" <| toString default
            , Html.on "input" Html.targetValue action
            ] []
        ]

intSlider name min max step default mbox =
    slider name min max step default (String.toInt >> Result.toMaybe >> Maybe.withDefault default >> Signal.message mbox.address)

floatSlider : String -> Float -> Float -> Float -> Float -> Signal.Mailbox Float -> Html
floatSlider name min max step default mbox =
    slider name min max step default (String.toFloat >> Result.toMaybe >> Maybe.withDefault default >> Signal.message mbox.address)

maybeIntSlider name min max step mbox =
    slider name min max step max (String.toInt >> Result.toMaybe >> Signal.message mbox.address)

maybeFloatSlider name min max step mbox =
    slider name min max step max (String.toFloat >> Result.toMaybe >> Signal.message mbox.address)