module Views.ProgressBar.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Shared.Constants as Constants
import Views.ProgressBar.View as View exposing (CssClasses(..))


styles : Stylesheet
styles =
    (stylesheet << namespace View.namespace)
        [ (.) Container
            [ displayFlex
            , position relative
            , width (pct 100)
            , padding2 (px 8) zero
            , property "align-items" "center"
            , property "justify-content" "center"
            ]
        , (.) BarOuter
            [ maxWidth (px 400)
            , width (pct 100)
            , position relative
            , border3 (px 1) solid (hex Colors.white)
            , height (px 32)
            , borderRadius (px 5)
            , overflow hidden
            ]
        , (.) BarInner
            [ position relative
            , height (pct 100)
            , backgroundColor (hex Colors.white)
            , property "transition" "width 0.2s linear"
            ]
        , (.) Count
            [ paddingLeft (px 16)
            , whiteSpace noWrap
            , fontSize (px 14)
            ]
        ]
