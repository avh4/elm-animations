module AnimationFrame where
{-| -- TODO:   This library is modeled after Elm's time module. It provides Signals that
are synchronized with the monitor's frame rate by binding javascript's
requestAnimationFrame. `AnimationFrame.frame` and `AnimationFrame.frameWhen` are similar
to `(Time.fps 60)` and `(Time.fpsWhen 60)` respectively, but they more reliably
fire once per frame.

#Tickers
@docs frameWhen
-}


import Native.AnimationFrame
import Signal
import Time

{-| Same as the frame function, but you can turn it on and off. Allows you
to do brief animations based on user input without major inefficiencies.
The values in the resulting signal are the timestamps of the frames.
-}
frameWhen : (Time.Time -> m -> Bool) -> Signal.Signal m -> Signal.Signal Time.Time
frameWhen = Native.AnimationFrame.frameWhen
