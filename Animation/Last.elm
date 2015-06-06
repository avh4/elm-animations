module Animation.Last where

import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Controls
import Easing exposing (Easing, Interpolation)
import Color
import Time exposing (Time)
import String
import Debug

-- --
-- -- TYPED HTML MODULE
-- --

toHexDigit i = case i of
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> toString i

toHexString i = (toHexDigit ((i//16)%16)) ++ (toHexDigit (i % 16))

toHtmlColor c = 
    let 
        {red,green,blue} = Color.toRgb c
    in
        "#" ++ (toHexString red) ++ (toHexString green) ++ (toHexString blue)

-- http://package.elm-lang.org/packages/identicalsnowflake/elm-typed-styles/2.0.0
width i = ("width", (toString i) ++ "px")
height i = ("height", (toString i) ++ "px")
background c = ("background", toHtmlColor c)

transform ts = ("transform", String.join " " ts)
rotate n = "rotate(" ++ (toString n) ++ "deg)"
scale n = "scale(" ++ (toString n) ++ ")"

-- --
-- -- EASING MODULE
-- --

-- https://github.com/Dandandan/Easing/pull/8
easeInt : Interpolation Int
easeInt from to v =
    from + (round ((toFloat (to-from)) * v))

-- --
-- -- ANIMATION MODULE
-- --

type alias Ease e = Interpolation e -> e -> e -> e
type alias AnimationState t = (Time,Time,Easing,Interpolation t,t,t)

animateOnOff : Time -> AnimationState Float -> Ease e
animateOnOff time state interp from to =
    currentValue time state |> interp from to

currentValue : Time -> AnimationState a -> a
currentValue t (start,end,easing,interp,from,to) =
    if  | t < start -> from
        | t > end -> to
        | otherwise -> Easing.ease easing interp from to (end-start) (t-start)

startAnimation : Easing -> Time -> Time -> a -> AnimationState a -> AnimationState a
startAnimation easing duration t v ((start0,end0,easing0,interp,v00,v0) as m) =
    if  | end0 < t -> (t,t+duration,easing,interp,v0,v)
        | otherwise -> (t,t+duration,easing,interp,currentValue t m,v)

startOnOffAnimation : Easing -> Time -> Time -> Bool -> AnimationState Float -> AnimationState Float
startOnOffAnimation easing duration t v =
    startAnimation easing duration t (if v then 1.0 else 0.0)

clearAnimation : a -> AnimationState a -> AnimationState a
clearAnimation v (_,_,_,interp,_,_) = (0, 1, Easing.linear, interp, v, v)

animationState : Interpolation a -> a -> AnimationState a
animationState interp v = (0, 1, Easing.linear, interp, v, v)

onOffAnimationState : Bool -> AnimationState Float
onOffAnimationState v = animationState Easing.float (if v then 1.0 else 0.0)

currentTargetValue : AnimationState a -> a
currentTargetValue (_, _, _, _, _, a) = a

currentOnOffTargetValue : AnimationState Float -> Bool
currentOnOffTargetValue (_, _, _, _, _, f) = f == 1.0

animationSignal : m -> ((Time,a) -> m -> m) -> (Time -> m -> h) -> Signal a -> Signal h
animationSignal init step render signal =
    let
        time = Time.fps 60 |> Time.timestamp |> Signal.map fst
        model = signal |> Time.timestamp |> Signal.foldp step init
    in
        Signal.map2 render time model