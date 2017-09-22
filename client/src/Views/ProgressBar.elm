module Views.ProgressBar exposing (Config, view)

import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (style)
import Views.ProgressBar.Styles as Styles


type alias Config =
    { percentage : Float
    , label : Maybe String
    }


viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf thunk predicate =
    case predicate of
        True ->
            thunk ()

        False ->
            text ""


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe innerView maybeValue =
    maybeValue
        |> Maybe.map innerView
        |> Maybe.withDefault (text "")


view : Config -> Html msg
view { percentage, label } =
    let
        widthPercentage =
            toString (round (percentage * 100)) ++ "%"
    in
    div [ Styles.container ]
        [ viewMaybe
            (\labelText -> div [ Styles.label ] [ text labelText ])
            label
        , div [ Styles.barContainer ]
            [ div [ Styles.barOuter ]
                [ div
                    [ Styles.barInner
                    , style [ ( "width", widthPercentage ) ]
                    ]
                    []
                ]
            , div [ Styles.count ]
                [ strong [] [ text <| widthPercentage ]
                ]
            ]
        ]
