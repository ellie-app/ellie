module Views.ProgressBar.View exposing (CssClasses(..), ViewModel, namespace, view)

import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (style)
import Html.CssHelpers


namespace : String
namespace =
    "components_progress_bar_"


type alias ViewModel =
    { total : Int
    , complete : Int
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


view : ViewModel -> Html msg
view { total, complete, label } =
    let
        widthPercentage =
            toString (round (toFloat complete / toFloat total * 100)) ++ "%"
    in
    div [ class [ Container ] ]
        [ viewMaybe
            (\labelText -> div [ class [ Label ] ] [ text labelText ])
            label
        , div [ class [ BarContainer ] ]
            [ div [ class [ BarOuter ] ]
                [ div
                    [ class [ BarInner ]
                    , style [ ( "width", widthPercentage ) ]
                    ]
                    []
                ]
            , div [ class [ Count ] ]
                [ strong [] [ text <| toString complete ]
                , text " / "
                , strong [] [ text <| toString total ]
                ]
            ]
        ]


{ class } =
    Html.CssHelpers.withNamespace "components_progress_bar_"


type CssClasses
    = Container
    | BarContainer
    | BarOuter
    | BarInner
    | Count
    | Label
