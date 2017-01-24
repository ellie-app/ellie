module Apps.Embed.Styles exposing (styles)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import Apps.Embed.Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "app_")
        [ html
            [ height (pct 100)
            ]
        , body
            [ height (pct 100)
            , margin zero
            , fontWeight (int 300)
            , fontFamilies [ Constants.sansFont ]
            ]
        , everything
            [ boxSizing borderBox ]
        , (.) Container
            [ width (pct 100)
            , height (pct 100)
            , position relative
            ]
        , (.) LoadingContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , alignItems center
            , padding (px 16)
            , paddingTop (px 100)
            , property "background-image" Colors.pinkPurpleGradient
            ]
        , (.) LoadingTitle
            [ fontFamilies [ Constants.scriptFont ]
            , color (hex Colors.white)
            , fontSize (px 48)
            , lineHeight (px 48)
            , padding (px 24)
            ]
        , (.) LoadingMessage
            [ fontSize (px 24)
            , color (hex Colors.white)
            ]
        , (.) FailureContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , alignItems center
            , padding (px 16)
            , paddingTop (px 100)
            , backgroundColor (hex Colors.mediumGray)
            ]
        , (.) FailureTitle
            [ fontFamilies [ Constants.scriptFont ]
            , color (hex Colors.white)
            , fontSize (px 48)
            , lineHeight (px 48)
            , paddingBottom (px 24)
            ]
        , (.) FailureMessage
            [ fontSize (px 24)
            , color (hex Colors.white)
            , padding2 zero (px 16)
            , maxWidth (px 528)
            , textAlign center
            , paddingBottom (px 24)
            ]
        , (.) FailureDetails
            [ maxWidth (px 600)
            , backgroundColor (hex Colors.darkGray)
            , color (hex Colors.white)
            , borderRadius (px 3)
            , padding (px 12)
            , fontSize (px 16)
            ]
        , (.) Header
            [ backgroundColor (hex Colors.pink)
            , padding2 (px 6) (px 8)
            , displayFlex
            , height (px 40)
            , property "justify-content" "space-between"
            , boxShadow5 (px 0) (px 2) (px 4) (px 0) (rgba 0 0 0 0.5)
            ]
        , (.) HeaderLeft
            [ displayFlex ]
        , (.) HeaderButton
            [ alignItems center
            , padding2 (px 6) (px 12)
            , color (hex Colors.white)
            , fontSize (px 16)
            , lineHeight (px 16)
            , textTransform uppercase
            , border zero
            , borderRadius (px 3)
            , cursor pointer
            , marginRight (px 8)
            , displayFlex
            , backgroundColor <|
                rgba
                    (.r Colors.darkGrayRgb)
                    (.g Colors.darkGrayRgb)
                    (.b Colors.darkGrayRgb)
                    0.24
            , disabled
                [ opacity (num 0.5)
                , cursor notAllowed
                ]
            , hover
                [ backgroundColor <|
                    rgba
                        (.r Colors.darkGrayRgb)
                        (.g Colors.darkGrayRgb)
                        (.b Colors.darkGrayRgb)
                        0.33
                ]
            , active
                [ backgroundColor <|
                    rgba
                        (.r Colors.darkGrayRgb)
                        (.g Colors.darkGrayRgb)
                        (.b Colors.darkGrayRgb)
                        0.5
                ]
            ]
        , (.) HeaderButtonIcon
            [ width (px 16)
            , height (px 16)
            , marginRight (px 8)
            ]
        , (.) HeaderLinkLogo
            [ fontFamilies [ Constants.scriptFont ]
            , property "text-transform" "none"
            , marginLeft (px 6)
            ]
        , (.) LoadedContainer
            [ width (pct 100)
            , height (pct 100)
            , position relative
            ]
        , (.) WorkArea
            [ property "height" "calc(100% - 40px)"
            , position relative
            ]
        , (.) WorkAreaTab
            [ position absolute
            , height (pct 100)
            , top zero
            , left zero
            , width (pct 100)
            ]
        , (.) WorkAreaTabHidden
            [ property "visibility" "collapse"
            ]
        ]
