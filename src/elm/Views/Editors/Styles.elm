module Views.Editors.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
import Views.Editors.Classes exposing (Classes(..))
import Shared.Constants as Constants


shimmerGradient : String
shimmerGradient =
    "linear-gradient(to right, rgba(255, 255, 255, 0) 20%, rgba(255, 255, 255, 0.9) 50%, rgba(255,255,255,0) 80%) "


styles : Stylesheet
styles =
    (stylesheet << namespace "components_editors_")
        [ (.) Loading
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , minHeight (px 350)
            , backgroundColor (hex "fff")
            , position relative
            , overflow hidden
            ]
        , (.) LoadingGutter
            [ width (px 29)
            , height (pct 100)
            , backgroundColor (hex "e5e1e5")
            , displayFlex
            , flexDirection column
            , borderRight3 (px 1) solid (hex "bdb7bd")
            ]
        , (.) LoadingLineNumber
            [ fontSize (px 12)
            , property "font-family" "monospace"
            , width (pct 100)
            , textAlign right
            , padding (px 8)
            , color (hex "bdb7bd")
            , property "user-select" "none"
            ]
        , (.) LoadingLines
            [ minWidth (px 421)
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , padding2 (px 0) (px 8)
            ]
        , (.) LoadingLine
            [ height (px 12)
            , margin2 (px 9) (px 0)
            , backgroundColor (hex "e5e1e5")
            , borderRadius (px 12)
            ]
        , (.) LoadingShimmer
            [ height (pct 120)
            , width (pct 100)
            , position absolute
            , property "background-image" shimmerGradient
            , top (pct -10)
            , property "animation-name" "shimmer"
            , property "animation-duration" "2s"
            , property "animation-fill-model" "forwards"
            , property "animation-timing-function" "ease"
            , property "animation-play-state" "running"
            , property "animation-iteration-count" "infinite"
            ]
        ]
