module MaterialDesign.Button where

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Animation exposing (..)
import Color exposing (Color)
import Easing
import Time exposing (Time)
import TypedStyles exposing (..)
import String
import Json.Decode as Decode

type alias Model =
    { hover : AnimationState Float
    , click : AnimationState Float
    , clickFade : AnimationState Float
    , clickLocation : (Int, Int)
    , color : Color
    , title : String
    }

type Action
    = Click (Int, Int)
    | Hover Bool

init : Color -> String -> Model
init color title =
    { hover = onOffAnimationState False
    , click = onOffAnimationState False
    , clickFade = onOffAnimationState False
    , clickLocation = (0,0)
    , color = color
    , title = title
    }

isActive : Time -> Model -> Bool
isActive t m =
    Animation.isActive t m.hover
    || Animation.isActive t m.click
    || Animation.isActive t m.clickFade

step : (Time, Action) -> Model -> Model
step (time,a) m = case a of
    Click (x,y) ->
        { m
        | clickLocation <- (x,y)
        , click <- m.click
            |> clearAnimation 0.0
            |> startOnOffAnimation Easing.easeOutQuad 400 0 time True
        , clickFade <- m.clickFade
            |> clearAnimation 1.0
            |> startOnOffAnimation Easing.easeInOutQuad 200 200 time False
        }
    Hover b ->
        { m | hover <- m.hover |> startOnOffAnimation Easing.linear 250 0 time b }


boxShadow : Int -> Float -> Float -> Int -> Color -> (String, String)
boxShadow hShadow vShadow blur spread color =
    ("box-shadow", (toString hShadow) ++ "px " ++ (toString vShadow) ++ "px " ++ (toString blur) ++ "px " ++ (toString spread) ++ "px " ++ (cssColor color))

boxShadows : List (String, String) -> (String,String)
boxShadows list =
    ("box-shadow", list |> List.map snd |> String.join ",")

lighten : Color -> Float -> Color
lighten color pct = let c = (Color.toHsl color) in
    Color.hsla c.hue c.saturation (c.lightness + pct) c.alpha

render : (Action -> Signal.Message) -> Time -> Model -> Html
render message time m =
    let
        hover = currentValue time m.hover
        click = currentValue time m.click
        fade = currentValue time m.clickFade
        (x,y) = m.clickLocation
        color0 = m.color
        color = Easing.color (color0) (lighten color0 0.05) hover
        hf a b = Easing.float a b (hover)
        style =
            [ background color
            , TypedStyles.height (48-8-8) px
            , lineHeight (48-8-8) px
            , minWidth 64 px
            , ("color", "white")
            , ("border", "none")
            , ("border-radius", "2px")
            , ("outline", "0")
            --, padding 8 px
            , ("padding", "0 2rem")
            , ("text-transform", "uppercase")
            , ("vertical-align", "middle")
            , ("-webkit-tap-highlight-color", "transparent")
            , ("display", "inline-block")
            , ("letter-spacing", "0.5px")
            , ("cursor", "pointer")
            , ("text-decoration", "none")
            , ("text-align", "center")
            , boxShadows
                [ boxShadow 0 (hf 2 5) (hf 5 11) 0 (Color.rgba 0 0 0 (hf 0.16 0.18))
                , boxShadow 0 (hf 2 4) (hf 10 15) 0 (Color.rgba 0 0 0 (hf 0.12 0.15))
                ]
            , ("overflow", "hidden")
            , ("user-select", "none")
            , ("-webkit-user-select", "none")
            , ("will-change", "opacity,transform")
            ]
        rippleStyle =
            [ ("background", "rgba(255, 255, 255, 0.45)")
            , ("position", "absolute")
            , ("border-radius", "50%")
            , ("width", "20px")
            , ("height", "20px")
            , ("margin-top", "-10px")
            , ("margin-left", "-10px")
            , ("opacity", fade |> toString)
            , ("pointer-events", "none")
            , ("top", (toString y) ++ "px")
            , ("left", (toString x) ++ "px")
            , ("transform", "scale(" ++ (15*click |> toString) ++ ")")
            ]
    in
        Html.div
            [ Html.style style
            , Html.on "mouseenter" (Decode.succeed True) (Hover >> message)
            , Html.on "mouseleave" (Decode.succeed False) (Hover >> message)
            , Html.on "click" decodeClickLocation (Click >> message)
            ]
            [ Html.text m.title
            , Html.div [Html.style rippleStyle] []
            ]

decodeClickLocation : Decode.Decoder (Int,Int)
decodeClickLocation =
    Decode.object2 (,)
        (Decode.object2 (-)
            (Decode.at ["pageX"] Decode.int)
            (Decode.at ["target", "offsetLeft"] Decode.int)
        )
        (Decode.object2 (-)
            (Decode.at ["pageY"] Decode.int)
            (Decode.at ["target", "offsetTop"] Decode.int)
        )

--.z-depth-0 {
--  box-shadow: none !important;
--}
--.z-depth-1{
--  box-shadow: 0 2px 5px 0 rgba(0, 0, 0, 0.16), 0 2px 10px 0 rgba(0, 0, 0, 0.12);
--}
--.z-depth-1-half{
--  box-shadow: 0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15);
--}
--.z-depth-2{
--  box-shadow: 0 8px 17px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
--}
--.z-depth-3{
--  box-shadow: 0 12px 15px 0 rgba(0, 0, 0, 0.24), 0 17px 50px 0 rgba(0, 0, 0, 0.19);
--}
--.z-depth-4{
--  box-shadow: 0 16px 28px 0 rgba(0, 0, 0, 0.22), 0 25px 55px 0 rgba(0, 0, 0, 0.21);
--}
--.z-depth-5{
--  box-shadow: 0 27px 24px 0 rgba(0, 0, 0, 0.2), 0 40px 77px 0 rgba(0, 0, 0, 0.22);
--}
