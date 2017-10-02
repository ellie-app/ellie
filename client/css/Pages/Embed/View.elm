module Pages.Embed.View exposing (..)

import Colors
import Constants
import Css exposing (..)
import Css.Elements exposing (..)
import Css.Extra exposing (..)
import Css.File exposing (..)


htmlStyles : Snippet
htmlStyles =
    html [ height (pct 100) ]


bodyStyles : Snippet
bodyStyles =
    body
        [ height (pct 100)
        , margin zero
        , fontWeight (int 300)
        , fontFamilies [ Constants.sansFont ]
        ]


everythingStyles : Snippet
everythingStyles =
    everything
        [ boxSizing borderBox ]


container : UniqueClass
container =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        , overflow hidden
        , borderRadius (px 3)
        ]


failureContainer : UniqueClass
failureContainer =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , displayFlex
        , flexDirection column
        , alignItems center
        , padding (px 16)
        , paddingTop (px 100)
        , backgroundColor (hex Colors.mediumGray)
        ]


failureTitle : UniqueClass
failureTitle =
    uniqueClass
        [ fontFamilies [ Constants.scriptFont ]
        , color (hex Colors.white)
        , fontSize (px 48)
        , lineHeight (px 48)
        , paddingBottom (px 24)
        ]


failureMessage : UniqueClass
failureMessage =
    uniqueClass
        [ fontSize (px 24)
        , color (hex Colors.white)
        , padding2 zero (px 16)
        , maxWidth (px 528)
        , textAlign center
        , paddingBottom (px 24)
        ]


failureDetails : UniqueClass
failureDetails =
    uniqueClass
        [ maxWidth (px 600)
        , backgroundColor Colors.darkGray_
        , color (hex Colors.white)
        , borderRadius (px 3)
        , padding (px 12)
        , fontSize (px 16)
        ]


header : UniqueClass
header =
    uniqueClass
        [ backgroundColor Colors.darkGray_
        , borderBottom3 (px 1) solid Colors.pink_
        , displayFlex
        , height (px 40)
        , property "justify-content" "space-between"
        , boxShadow4 zero zero (px 5) (rgba 57 70 78 0.7)
        , property "z-index" "1"
        , position relative
        ]


headerLeft : UniqueClass
headerLeft =
    uniqueClass
        [ displayFlex ]


headerRight : UniqueClass
headerRight =
    uniqueClass
        [ padding (px 6) ]


headerTab : UniqueClass
headerTab =
    uniqueClass
        [ padding2 zero (px 12)
        , paddingTop (px 2)
        , height (pct 100)
        , color (hex Colors.white)
        , fontSize (px 16)
        , lineHeight (px 16)
        , border zero
        , cursor pointer
        , margin zero
        , textDecoration none
        , outline zero
        , fontFamily inherit
        , property "-webkit-appearance" "none"
        , lastChild
            [ marginRight zero
            ]
        , backgroundColor transparent
        , disabled
            [ opacity (num 0.5)
            , cursor notAllowed
            ]
        , hover
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.2
            ]
        , active
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.35
            ]
        ]


headerTabActive : UniqueClass
headerTabActive =
    uniqueClass
        [ borderTop3 (px 2) solid (hex Colors.pink)
        , property "border-image" <| Colors.pinkPurpleGradient ++ " 1"
        , paddingTop zero
        , color (hex Colors.white)
        , backgroundColor <|
            rgba
                (.r Colors.mediumGrayRgb)
                (.g Colors.mediumGrayRgb)
                (.b Colors.mediumGrayRgb)
                0.5
        ]


headerLink : UniqueClass
headerLink =
    uniqueClass
        [ padding2 (px 6) (px 12)
        , borderRadius (px 3)
        , color (hex Colors.white)
        , fontSize (px 16)
        , lineHeight (px 16)
        , textTransform uppercase
        , border zero
        , cursor pointer
        , margin zero
        , textDecoration none
        , outline zero
        , fontFamily inherit
        , property "-webkit-appearance" "none"
        , backgroundColor <|
            rgba
                (.r Colors.mediumGrayRgb)
                (.g Colors.mediumGrayRgb)
                (.b Colors.mediumGrayRgb)
                0.3
        , hover
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.5
            ]
        , active
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.7
            ]
        ]


headerTabInner : UniqueClass
headerTabInner =
    uniqueClass
        [ alignItems center
        , displayFlex
        ]


headerLinkInner : UniqueClass
headerLinkInner =
    uniqueClass
        [ alignItems center
        , displayFlex
        ]


headerTabIcon : UniqueClass
headerTabIcon =
    uniqueClass
        [ width (px 16)
        , height (px 16)
        , marginRight (px 8)
        ]


headerLinkIcon : UniqueClass
headerLinkIcon =
    uniqueClass
        [ width (px 16)
        , height (px 16)
        , marginRight (px 8)
        ]


headerLinkLogo : UniqueSvgClass
headerLinkLogo =
    uniqueSvgClass
        [ marginLeft (px 6)
        , width (px 41)
        , height (px 16)
        , fill Colors.lightGray_
        ]


loadedContainer : UniqueClass
loadedContainer =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , property "transition" "filter 0.3s 0.2s"
        ]


loadingContainer : UniqueClass
loadingContainer =
    uniqueClass
        [ filter <| blur (px 30)
        ]


workArea : UniqueClass
workArea =
    uniqueClass
        [ property "height" "calc(100% - 40px)"
        , position relative
        ]


workAreaTab : UniqueClass
workAreaTab =
    uniqueClass
        [ position absolute
        , height (pct 100)
        , top zero
        , left zero
        , width (pct 100)
        , overflow hidden
        ]


workAreaTabHidden : UniqueClass
workAreaTabHidden =
    uniqueClass
        [ property "visibility" "collapse"
        ]


iframe : UniqueClass
iframe =
    uniqueClass
        [ border zero
        , position relative
        , width (pct 100)
        , backgroundColor (hex "fff")
        , height (pct 100)
        ]


errors =
    uniqueClass
        [ displayFlex
        , padding (px 16)
        , flexDirection column
        , alignItems center
        , position relative
        ]


error =
    uniqueClass
        [ marginBottom (px 16)
        , lastChild [ marginBottom (px 16) ]
        , maxWidth (px 500)
        , width (pct 100)
        ]
