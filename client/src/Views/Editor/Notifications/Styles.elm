module Views.Editor.Notifications.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Shared.Constants as Constants
import Views.Editor.Notifications.Classes exposing (Classes(..))


styles : Stylesheet
styles =
    (stylesheet << namespace "components_notifications_")
        [ Css.class Popout
            [ position relative
            , backgroundColor (hex Colors.mediumGray)
            , width (px 400)
            , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
            , property "z-index" "4"
            , borderRadius (px 3)
            , padding (px 16)
            , overflowY auto
            , maxHeight (px 400)
            , border3 (px 2) solid (hex Colors.lightGray)
            ]
        , Css.class Item
            [ backgroundColor (hex Colors.darkGray)
            , marginBottom (px 2)
            , padding (px 12)
            , firstChild
                [ borderTopRightRadius (px 3)
                , borderTopLeftRadius (px 3)
                ]
            , lastChild
                [ borderBottomLeftRadius (px 3)
                , borderBottomRightRadius (px 3)
                , marginBottom zero
                ]
            ]
        , Css.class ItemDetails
            [ displayFlex
            , property "align-items" "center"
            , color (hex Colors.white)
            , property "white-space" "pre-wrap"
            ]
        , Css.class ItemMessage
            [ property "width" "calc(100% - 24px)"
            , paddingRight (px 16)
            ]
        , Css.class ItemIcon
            [ width (px 24)
            ]
        , Css.class ItemTitle
            [ fontSize (px 12)
            , textTransform uppercase
            , paddingBottom (px 4)
            , displayFlex
            , color (hex Colors.lightGray)
            , property "justify-content" "space-between"
            ]
        , Css.class ItemTimestamp
            []
        ]
