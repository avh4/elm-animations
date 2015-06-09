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
import AnimationFrame

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

toHexString i' =
    let i = i' |> max 0 |> min 255
    in (toHexDigit ((i//16)%16)) ++ (toHexDigit (i % 16))

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
type alias Animator t s =
    { zero: t
    , add: t -> s -> s
    , diff: s -> s -> t
    , interpolation: Interpolation t
    }
type alias ComplexAnimationState t s = -- TODO: are there any reasonable use cases where t and s are different?
    { animations: List (Time,Time,Easing,t)
    , current: s
    , animator: Animator t s
    }
type alias AnimationState a = ComplexAnimationState a a

animateOnOff : Time -> AnimationState Float -> Ease e
animateOnOff time state interp from to =
    currentValue time state |> interp from to

currentValue : Time -> ComplexAnimationState t s -> s
currentValue t state =
    let
        to = state.animator.zero
        interp = state.animator.interpolation
        val (start,end,easing,from) =
            if  | t < start -> from
                | t > end -> to
                | otherwise -> Easing.ease easing interp from to (end-start) (t-start)
        add animation s =
            state.animator.add (val animation) s
    in
        List.foldr add state.current state.animations

startAnimation : Easing -> Time -> Time -> Time -> s -> ComplexAnimationState t s -> ComplexAnimationState t s
startAnimation easing duration delay now v state =
    let
        start = now + delay
        end = start + duration
        isActive (_,end',_,_) = now <= end'
    in
        { state
        | current <- v
        , animations <- (start,end,easing,state.animator.diff state.current v) :: (List.filter isActive state.animations)
        }

startOnOffAnimation : Easing -> Time -> Time -> Time -> Bool -> AnimationState Float -> AnimationState Float
startOnOffAnimation easing duration delay now v =
    startAnimation easing duration delay now (if v then 1.0 else 0.0)

clearAnimation : a -> ComplexAnimationState t a -> ComplexAnimationState t a
clearAnimation v state =
    { state
    | current <- v
    , animations <- []
    }

animationState : Animator t s -> s -> ComplexAnimationState t s
animationState animator initialValue =
    { animations = []
    , current = initialValue
    , animator = animator
    }

float : Animator Float Float
float = { interpolation = Easing.float, zero = 0, add = (+), diff = (-) }

int : Animator Int Int
int = { interpolation = easeInt, zero = 0, add = (+), diff = (-) }

pair : Animator a b -> Animator (a,a) (b,b)
pair single =
    { interpolation = Easing.pair single.interpolation
    , zero = (single.zero, single.zero)
    , add = \(a,b) (x,y) -> (single.add a x, single.add b y)
    , diff = \(a,b) (x,y) -> (single.diff a x, single.diff b y)
    }

onOffAnimationState : Bool -> AnimationState Float
onOffAnimationState v = animationState float (if v then 1.0 else 0.0)

currentTargetValue : ComplexAnimationState t a -> a
currentTargetValue state = state.current

currentOnOffTargetValue : AnimationState Float -> Bool
currentOnOffTargetValue = currentTargetValue >> (==) 1.0

isActive : Time -> ComplexAnimationState t a -> Bool
isActive t state = List.any (\(_,end,_,_) -> t <= end) state.animations

animationSignal : m -> ((Time,a) -> m -> m) -> (Time -> m -> h) -> (Time -> m -> Bool) -> Signal a -> Signal h
animationSignal init step render activeAnimations signal =
    let
        model = signal |> Time.timestamp |> Signal.foldp step init
        time = AnimationFrame.frameWhen activeAnimations model
    in
        Signal.map2 render time model
