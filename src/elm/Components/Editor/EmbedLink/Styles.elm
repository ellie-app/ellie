module Components.Editor.EmbedLink.Styles
    exposing
        ( styles
        )

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
import Components.Editor.EmbedLink.Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "components_editor_embedLink_")
        [ (.) Container
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
        , (.) Option
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
        , (.) OptionTitle
            [ fontSize (px 12)
            , lineHeight (px 12)
            , textTransform uppercase
            , color (hex Colors.lightGray)
            ]
        , (.) OptionContent
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
        ]
