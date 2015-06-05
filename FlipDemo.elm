import Html exposing (Html)
import Html.Attributes as Html
import Time

-- TODO: yellowCircle fades in and out
-- TODO: largeBlueBox/smallRedBox positions/sizes transition
-- TODO: largeBlueBox/smallRedBox cross fade during transition

yellowCircle : Html
yellowCircle = Html.img [Html.src "yellowCircle.png"] []

largeBlueBox : Html
largeBlueBox = 
    let
        style =
            [ ("background", "#77f")
            , ("width", "250px")
            , ("height", "250px")
            , ("transition", "width 1s, height 1s, background 1s")
            ]
    in
        Html.div [Html.style style] []

smallRedBox : Html
smallRedBox = 
    let
        style =
            [ ("background", "#f77")
            , ("width", "40px")
            , ("height", "40px")
            , ("margin", "20px")
            , ("transition", "width 1s, height 1s, background 1s")
            ]
    in
        Html.div [Html.style style] []

render : Bool -> Html
render b =
    if b
        then
            Html.div []
            [ largeBlueBox
            , yellowCircle
            ] |> fadeIn 0.5
        else
            smallRedBox |> fadeIn 0.5

fadeIn m d html = animation m
    { view: \t _ -> html |> withOpacity t
    , duration: d
    }

step : a -> Bool -> Bool
step _ m = case m of
    True -> False
    False -> True

main = Signal.map render (Time.every 1000 |> Signal.foldp step True)




{-
    ANIMATION CASES

      - an element changes explicit size
      - an element changes color
-}
