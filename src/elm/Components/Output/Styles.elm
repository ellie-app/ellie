module Components.Output.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
import Components.Output.Classes as Classes exposing (Classes(..))
import Shared.Constants as Constants


shimmerGradient : String
shimmerGradient =
    "linear-gradient(to right, rgba(255, 255, 255, 0) 20%, rgba(255, 255, 255, 0.9) 50%, rgba(255,255,255,0) 80%) "


styles : Stylesheet
styles =
    (stylesheet << namespace "components_output_")
        [ (.) LoadingShimmer
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
        , (.) Loading
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , padding (px 16)
            , flexDirection column
            , overflow hidden
            , position relative
            ]
        , (.) LoadingSection
            [ marginBottom (px 32)
            , position relative
            ]
        , (.) LoadingFullBox
            [ width (pct 100)
            , height (px 80)
            , marginBottom (px 12)
            , backgroundColor (hex "e5e1e5")
            ]
        , (.) LoadingSplitContainer
            [ width (pct 100)
            , displayFlex
            ]
        , (.) LoadingSplitLeft
            [ width (pct 60)
            , paddingRight (px 8)
            ]
        , (.) LoadingSplitRight
            [ width (pct 40)
            , paddingLeft (px 8)
            , textAlign center
            ]
        , (.) LoadingCircle
            [ height (pct 100)
            , borderRadius (pct 50)
            , backgroundColor (hex "e5e1e5")
            , width (pct 100)
            , maxWidth (px 184)
            , display inlineBlock
            ]
        , (.) Iframe
            [ width (pct 100)
            , height (pct 100)
            , border zero
            ]
        , (.) Overlay
            [ width (pct 100)
            , height (pct 100)
            , position relative
            , backgroundColor (hex "55B5DB")
            , displayFlex
            , property "align-items" "center"
            , property "justify-content" "center"
            , flexDirection column
            , color (hex "fff")
            , padding (px 16)
            ]
        , (.) OverlayTitle
            [ property "font-family" "Leckerli One"
            , fontSize (px 48)
            , marginBottom (px 16)
            ]
        , (.) OverlaySubtitle
            [ fontSize (px 24)
            , textAlign center
            ]
        , (.) ErrorsContainer
            [ padding (px 12)
            , width (pct 100)
            ]
        , (.) ErrorItem
            [ padding (px 12)
            , boxShadow5 (px 1) (px 3) (px 13) (px -2) (rgba 0 0 0 0.4)
            , backgroundColor (hex "e00")
            , color (hex "fff")
            , descendants
                [ code
                    [ fontWeight (int 700)
                    , backgroundColor (rgba 255 255 255 0.4)
                    , padding2 (px 2) (px 6)
                    , borderRadius (px 3)
                    , display inlineBlock
                    , lineHeight (int 1)
                    ]
                ]
            ]
        , (.) ErrorItemHeader
            [ marginBottom (px 12)
            , paddingBottom (px 8)
            , fontSize (px 14)
            , borderBottom3 (px 3) solid (hex "fff")
            , displayFlex
            , property "justify-content" "space-between"
            ]
        , (.) ErrorItemName
            []
        , (.) ErrorItemLocation
            [ textTransform uppercase ]
        , (.) ErrorItemOverview
            [ fontSize (px 20)
            , marginBottom (px 18)
            , fontWeight (int 700)
            ]
        , (.) ErrorItemDetails
            [ fontSize (px 16)
            ]
        ]
