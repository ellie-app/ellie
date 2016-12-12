module Views.Icons exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (svg, path)
import Svg.Attributes exposing (viewBox, d, fillRule, stroke)


playOutline : Html msg
playOutline =
    svg
        [ viewBox "0 0 15 18"
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "position", "relative" )
            ]
        ]
        [ path
            [ d "M0.880434783,1.625625 L12.4251359,9 L0.880434783,16.374375 L0.880434783,1.625625 L0.880434783,1.625625 Z M0,0 L0,18 L14.0869565,9 L0,0 L0,0 Z"
            , fillRule "evenodd"
            ]
            []
        ]
