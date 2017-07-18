module Views.Editor.EmbedLink.Styles
    exposing
        ( styles
        )

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Shared.Constants as Constants
import Views.Editor.EmbedLink.Classes exposing (Classes(..))


styles : Stylesheet
styles =
    (stylesheet << namespace "components_editor_embedLink_")
        [ Css.class Container
            [ position relative
            , backgroundColor (hex Colors.mediumGray)
            , width (pct 100)
            , height (pct 100)
            , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
            , border3 (px 1) solid (hex Colors.lightGray)
            , borderRadius (px 3)
            , padding (px 16)
            , position relative
            , color (hex Colors.white)
            , fontSize (px 14)
            , overflowY auto
            ]
        , Css.class Links []
        , Css.class Link
            [ backgroundColor (hex Colors.darkGray)
            , marginBottom (px 2)
            , padding3 (px 8) (px 8) (px 16)
            , firstChild
                [ borderTopLeftRadius (px 3)
                , borderTopRightRadius (px 3)
                ]
            , lastChild
                [ borderBottomLeftRadius (px 3)
                , borderBottomRightRadius (px 3)
                , marginBottom (px 2)
                ]
            ]
        , Css.class LinkTitle
            [ fontSize (px 12)
            , lineHeight (px 12)
            , textTransform uppercase
            , color (hex Colors.lightGray)
            ]
        , Css.class LinkContent
            [ paddingTop (px 8)
            , whiteSpace noWrap
            , overflowX hidden
            , fontFamily monospace
            , property "-webkit-appearance" "none"
            , property "background" "none"
            , border zero
            , display block
            , width (pct 100)
            , fontSize (px 14)
            , color (hex Colors.white)
            ]
        , Css.class Buttons
            [ paddingTop (px 16)
            ]
        , Css.class Button
            [ display block
            , backgroundColor (hex Colors.darkGray)
            , color (hex Colors.lightGray)
            , borderRadius (px 3)
            , border zero
            , width (pct 100)
            , padding (px 12)
            , fontSize (px 16)
            , textTransform uppercase
            , fontFamily inherit
            , fontWeight (int 500)
            , cursor pointer
            , disabled
                [ opacity (num 0.5)
                ]
            ]
        , Css.class ButtonInner
            [ displayFlex
            , property "justify-content" "center"
            , alignItems center
            ]
        , Css.class ButtonIcon
            [ width (px 20)
            , height (px 20)
            , adjacentSiblings
                [ Css.class ButtonText
                    [ marginLeft (px 12)
                    ]
                ]
            ]
        ]
