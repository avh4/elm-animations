module Controls where

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Result
import Maybe
import String
import Signal

type alias Slider a =
    { html : Html
    , signal : Signal a
    }

slider : String -> a -> a -> a -> a -> (String -> a) -> Slider a
slider name min max step default parse =
    let
        mbox = Signal.mailbox default
        html = 
            Html.label []
                [ Html.text name
                , Html.input
                    [ Html.type' "range"
                    , Html.min <| toString min
                    , Html.max <| toString max
                    , Html.step <| toString step
                    , Html.attribute "value" <| toString default
                    , Html.on "input" Html.targetValue (parse >> Signal.message mbox.address)
                    ] []
                ]
    in
        { html = html, signal = mbox.signal }

intSlider : String -> Int -> Int -> Int -> Int -> Slider Int
intSlider name min max step default =
    slider name min max step default (String.toInt >> Result.toMaybe >> Maybe.withDefault default)

floatSlider : String -> Float -> Float -> Float -> Float -> Slider Float
floatSlider name min max step default =
    slider name min max step default (String.toFloat >> Result.toMaybe >> Maybe.withDefault default)

--maybeIntSlider name min max step mbox =
--    slider name min max step max (String.toInt >> Result.toMaybe >> Signal.message mbox.address)

--maybeFloatSlider name min max step mbox =
--    slider name min max step max (String.toFloat >> Result.toMaybe >> Signal.message mbox.address)