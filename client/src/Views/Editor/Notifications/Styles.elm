module Views.Editor.Notifications.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Views.Editor.Notifications.Classes exposing (Classes(..))


styles : Stylesheet
styles =
    (stylesheet << namespace "components_notifications_")
        [ Css.class Notifications
            [ position absolute
            , top zero
            , right zero
            , width (px 400)
            , zIndex (int 10)
            , padding (px 16)
            , maxHeight (pct 100)
            , overflowY auto
            ]
        , Css.class Item
            [ backgroundColor (hex Colors.darkGray)
            , marginBottom (px 8)
            , padding (px 16)
            , position relative
            , color (hex Colors.white)
            , property "align-items" "center"
            , property "box-shadow" "-4px 4px 10px -4px rgba(0, 0, 0, 0.5)"
            , borderRadius (px 3)
            , lastChild [ marginBottom zero ]
            ]
        , Css.class ItemMessage
            [ property "white-space" "pre-wrap"
            , fontWeight normal
            , fontSize (px 16)
            , overflowX auto
            ]
        , Css.class ItemTitle
            [ fontSize (px 12)
            , textTransform uppercase
            , fontWeight bold
            ]
        , Css.class ItemTimestamp
            [ fontSize (px 12)
            , paddingBottom (px 12)
            ]
        , Css.class CloseButton
            [ position absolute
            , top (px 16)
            , right (px 16)
            , width (px 12)
            , backgroundColor transparent
            , border zero
            , outline zero
            , padding zero
            , color inherit
            , cursor pointer
            ]
        ]
