module Ellie.Ui.ProgressBar exposing (..)

import Colors
import Css exposing (..)
import Html.Styled exposing (Html, Attribute, div)
import Html.Styled.Attributes exposing (css, style)


type Progress
    = Indeterminate
    | Percentage Float


viewIndeterminate : Html msg
viewIndeterminate =
    div [ outerStyles ]
        [ div [ innerIndeterminateStyles ] []
        ]


view : Progress -> Html msg
view progress =
    case progress of
        Indeterminate ->
            viewIndeterminate

        Percentage percentage ->
            div [ outerStyles ]
                [ div [ innerPercentageStyles percentage ] []
                ]


-- STYLES


outerStyles : Attribute msg
outerStyles =
    css
        [ width (pct 100)
        , height (px 8)
        , overflowX hidden
        , backgroundColor Colors.mediumGray
        ]


innerIndeterminateStyles : Attribute msg
innerIndeterminateStyles =
    css
        [ property "animation" "1.5s infinite ProgressBar-indeterminate"
        , property "transform-origin" "left"
        , height (px 8)
        , width (pct 100)
        , backgroundColor Colors.pink
        ]


innerPercentageStyles : Float -> Attribute msg
innerPercentageStyles percentage =
    css
        [ property "transform-origin" "left"
        , height (px 8)
        , property "transition" "transform 150ms"
        , width (pct 100)
            , property "transform" <| "scaleX(" ++ toString percentage ++ ")"
        , backgroundColor Colors.pink
        ]
