module Animation.Last where

import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Controls
import Easing
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
easeInt : Easing.Interpolation Int
easeInt from to v =
    from + (round ((toFloat (to-from)) * v))

-- --
-- -- ANIMATION MODULE
-- --

type alias Ease a = Easing.Interpolation a -> a -> a -> a
type alias Animation t a = Time -> AnimationState t -> Ease a
type alias AnimationState a = (Time,Time,a,a)

animateOnOff : Float -> Easing.Easing -> Animation Bool a
animateOnOff delay easing t (start,end,_,v) = --TODO: use from
    let
        duration = end - start
        t' = case v of
            True -> (t - delay) - start |> max 0
            False -> (duration - ((t - delay) - start)) |> max 0
    in
        \interp from to -> Easing.ease easing interp from to duration t'

animateIntTuple : Easing.Easing -> Time -> AnimationState (Int,Int) -> (Int,Int)
animateIntTuple easing t (start,end,from,to) =
    Easing.ease easing (Easing.pair easeInt) from to (end-start) (t-start)

startAnimation : Time -> Time -> a -> AnimationState a -> AnimationState a
startAnimation duration t v (start0,end0,v00,v0) =
    if  | end0 < t -> (t,t+duration,v0,v)
        | otherwise -> 
            let t' = t-(end0-t)
            in (t',t'+duration,v0,v)

animationState : a -> AnimationState a
animationState a = (0, 1, a, a)

currentValue : AnimationState a -> a
currentValue (_, _, _, a) = a

animationSignal : m -> ((Time,a) -> m -> m) -> (Time -> m -> h) -> Signal a -> Signal h
animationSignal init step render signal =
    let
        time = Time.fps 60 |> Time.timestamp |> Signal.map fst
        model = signal |> Time.timestamp |> Signal.foldp step init
    in
        Signal.map2 render time model