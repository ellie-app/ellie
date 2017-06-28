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
    }


viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf thunk predicate =
    case predicate of
        True ->
            thunk ()

        False ->
            text ""


view : ViewModel -> Html msg
view { total, complete } =
    let
        widthPercentage =
            toString (round (toFloat complete / toFloat total * 100)) ++ "%"
    in
    div [ class [ Container ] ]
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


{ class } =
    Html.CssHelpers.withNamespace "components_progress_bar_"


type CssClasses
    = Container
    | BarOuter
    | BarInner
    | Count
