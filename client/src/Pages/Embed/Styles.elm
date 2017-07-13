module Pages.Embed.Styles exposing (styles)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import Pages.Embed.Classes exposing (Classes(..))
import Shared.Colors as Colors
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "app_")
        [ html
            [ height (pct 100)
            , backgroundColor (hex Colors.darkGray)
            ]
        , body
            [ height (pct 100)
            , margin zero
            , fontWeight (int 300)
            , fontFamilies [ Constants.sansFont ]
            ]
        , everything
            [ boxSizing borderBox ]
        , Css.class Container
            [ width (pct 100)
            , height (pct 100)
            , position relative
            , property "z-index" "1"
            , overflow hidden
            , borderRadius (px 3)
            ]
        , Css.class LoadingContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , alignItems center
            , padding (px 16)
            , paddingTop (px 100)
            , property "background-image" Colors.pinkPurpleGradient
            ]
        , Css.class LoadingTitle
            [ fontFamilies [ Constants.scriptFont ]
            , color (hex Colors.white)
            , fontSize (px 48)
            , lineHeight (px 48)
            , padding (px 24)
            ]
        , Css.class LoadingMessage
            [ fontSize (px 24)
            , color (hex Colors.white)
            ]
        , Css.class FailureContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , alignItems center
            , padding (px 16)
            , paddingTop (px 100)
            , backgroundColor (hex Colors.mediumGray)
            ]
        , Css.class FailureTitle
            [ fontFamilies [ Constants.scriptFont ]
            , color (hex Colors.white)
            , fontSize (px 48)
            , lineHeight (px 48)
            , paddingBottom (px 24)
            ]
        , Css.class FailureMessage
            [ fontSize (px 24)
            , color (hex Colors.white)
            , padding2 zero (px 16)
            , maxWidth (px 528)
            , textAlign center
            , paddingBottom (px 24)
            ]
        , Css.class FailureDetails
            [ maxWidth (px 600)
            , backgroundColor (hex Colors.darkGray)
            , color (hex Colors.white)
            , borderRadius (px 3)
            , padding (px 12)
            , fontSize (px 16)
            ]
        , Css.class Header
            [ backgroundColor (hex Colors.darkGray)
            , borderBottom3 (px 1) solid (hex Colors.pink)
            , property "border-image" <| Colors.pinkPurpleGradient ++ " 1"
            , displayFlex
            , height (px 40)
            , property "justify-content" "space-between"
            , boxShadow4 zero zero (px 5) (rgba 57 70 78 0.7)
            , property "z-index" "1"
            , position relative
            ]
        , Css.class HeaderLeft
            [ displayFlex ]
        , Css.class HeaderRight
            [ padding (px 6) ]
        , Css.class HeaderTab
            [ padding2 zero (px 12)
            , paddingTop (px 2)
            , height (pct 100)
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
            , withClass HeaderTabActive
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
            ]
        , Css.class HeaderLink
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
        , Css.class HeaderTabInner
            [ alignItems center
            , displayFlex
            ]
        , Css.class HeaderLinkInner
            [ alignItems center
            , displayFlex
            ]
        , Css.class HeaderTabIcon
            [ width (px 16)
            , height (px 16)
            , marginRight (px 8)
            ]
        , Css.class HeaderLinkIcon
            [ width (px 16)
            , height (px 16)
            , marginRight (px 8)
            ]
        , Css.class HeaderLinkLogo
            [ fontFamilies [ Constants.scriptFont ]
            , property "text-transform" "none"
            , marginLeft (px 6)
            ]
        , Css.class LoadedContainer
            [ width (pct 100)
            , height (pct 100)
            , position relative
            ]
        , Css.class WorkArea
            [ property "height" "calc(100% - 40px)"
            , position relative
            ]
        , Css.class WorkAreaTab
            [ position absolute
            , height (pct 100)
            , top zero
            , left zero
            , width (pct 100)
            , overflow hidden
            ]
        , Css.class WorkAreaTabHidden
            [ property "visibility" "collapse"
            ]
        , Css.class Iframe
            [ border zero
            , position relative
            , width (pct 100)
            , height (pct 100)
            ]
        ]
