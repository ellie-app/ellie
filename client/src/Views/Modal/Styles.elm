module Views.Modal.Styles exposing (styles)

import Css exposing (..)
import Css.Namespace exposing (..)
import Extra.Css as Css exposing (..)
import Shared.Colors as Colors
import Views.Modal.Classes exposing (Classes(..))


styles : Stylesheet
styles =
    (stylesheet << namespace "Views-Modal-")
        [ class Container
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , top zero
            , left zero
            , zIndex (int 5)
            , displayFlex
            , flexDirection column
            , alignItems center
            , paddingTop (pct 25)
            ]
        , class Backdrop
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , top zero
            , left zero
            , zIndex (int 1)
            , backdropFilter <| blur (px 10)
            , backgroundColor <|
                rgba
                    (.r Colors.lightGrayRgb)
                    (.g Colors.lightGrayRgb)
                    (.b Colors.lightGrayRgb)
                    0.75
            ]
        , class Content
            [ width (pct 50)
            , zIndex (int 2)
            , backgroundColor (hex Colors.darkGray)
            , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.5)
            , borderRadius (px 3)
            ]
        ]
