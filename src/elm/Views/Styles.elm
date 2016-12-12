module Views.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
import Views.Classes as Classes exposing (Classes(..))


shimmerGradient : String
shimmerGradient =
    "linear-gradient(to right, rgba(255, 255, 255, 0) 20%, rgba(255, 255, 255, 0.9) 50%, rgba(255,255,255,0) 80%) "


styles : Stylesheet
styles =
    (stylesheet << namespace "views")
        [ (.) Header
            [ width (pct 100)
            , height (px 60)
            , backgroundColor (hex "f7f7f7")
            , borderBottom3 (px 1) solid (hex "55B5DB")
            , displayFlex
            , alignItems center
            ]
        , (.) HeaderLogo
            [ padding2 zero (px 16)
            ]
        , (.) HeaderLogoText
            [ property "font-family" "Leckerli One"
            , margin zero
            ]
        , (.) HeaderStatus
            [ padding2 zero (px 8)
            ]
        , (.) HeaderStatusText
            [ textTransform uppercase
            , fontSize (em 1.1)
            ]
        , (.) HeaderButton
            [ property "-webkit-appearance" "none"
            , backgroundColor transparent
            , border zero
            , padding2 zero (px 8)
            , margin2 zero (px 8)
            , fontFamily inherit
            , fontSize (em 1.1)
            , textTransform uppercase
            , fontWeight inherit
            , cursor pointer
            , outline zero
            , height (pct 100)
            , borderBottom3 (px 3) solid (hex "A7A7A7")
            , minWidth (px 120)
            , displayFlex
            , alignItems center
            , property "justify-content" "center"
            , hover
                [ borderBottomColor (hex "55B5DB")
                ]
            ]
        , (.) HeaderButtonIcon
            [ height (px 20)
            , width (px 20)
            , display inlineBlock
            , marginRight (px 6)
            , position relative
            ]
        , (.) LoadingEditor
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , minHeight (px 350)
            , backgroundColor (hex "fff")
            , position relative
            , overflow hidden
            ]
        , (.) LoadingEditorGutter
            [ width (px 29)
            , height (pct 100)
            , backgroundColor (hex "e5e1e5")
            , displayFlex
            , flexDirection column
            , borderRight3 (px 1) solid (hex "bdb7bd")
            ]
        , (.) LoadingEditorLineNumber
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
        , (.) LoadingEditorLine
            [ height (px 12)
            , margin2 (px 9) (px 0)
            , backgroundColor (hex "e5e1e5")
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
        , (.) LoadingResults
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , padding (px 16)
            , flexDirection column
            , overflow hidden
            , position relative
            ]
        , (.) LoadingResultsSection
            [ marginBottom (px 32)
            , position relative
            ]
        , (.) LoadingResultsFullBox
            [ width (pct 100)
            , height (px 80)
            , marginBottom (px 12)
            , backgroundColor (hex "e5e1e5")
            ]
        , (.) LoadingResultsSplitContainer
            [ width (pct 100)
            , displayFlex
            ]
        , (.) LoadingResultsSplitLeft
            [ width (pct 60)
            , paddingRight (px 8)
            ]
        , (.) LoadingResultsSplitRight
            [ width (pct 40)
            , paddingLeft (px 8)
            , textAlign center
            ]
        , (.) LoadingResultsCircle
            [ height (pct 100)
            , borderRadius (pct 50)
            , backgroundColor (hex "e5e1e5")
            , width (pct 100)
            , maxWidth (px 184)
            , display inlineBlock
            ]
        ]
