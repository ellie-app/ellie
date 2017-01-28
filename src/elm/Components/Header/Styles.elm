module Components.Header.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Header.Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "components_header_")
        [ (.) Header
            [ width (pct 100)
            , height (px Constants.headerHeight)
            , backgroundColor (hex Colors.pink)
            , displayFlex
            , alignItems center
            , boxShadow5 (px 0) (px 2) (px 15) (px -4) (rgba 0 0 0 0.5)
            , position relative
            , property "z-index" "4"
            , property "justify-content" "space-between"
            , padding2 zero (px 16)
            ]
        , (.) HeaderGroup
            [ displayFlex
            , alignItems center
            ]
        , (.) Logo
            [ fontFamilies [ Constants.scriptFont ]
            , fontSize (px 32)
            , color (hex Colors.white)
            , paddingRight (px 24)
            ]
        , (.) Button
            [ backgroundColor <|
                rgba
                    (.r Colors.darkGrayRgb)
                    (.g Colors.darkGrayRgb)
                    (.b Colors.darkGrayRgb)
                    0.24
            , border zero
            , borderRadius (px 3)
            , color (hex Colors.white)
            , fontFamily inherit
            , textTransform uppercase
            , height (px 38)
            , fontSize (px 14)
            , padding2 zero (px 12)
            , cursor pointer
            , marginLeft (px 8)
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
        , (.) ButtonIcon
            [ height (px 16)
            , width (px 16)
            , display block
            ]
        , (.) ButtonText
            [ marginLeft (px 8)
            ]
        , (.) ButtonInner
            [ displayFlex
            , property "justify-content" "center"
            , alignItems center
            ]
        ]
