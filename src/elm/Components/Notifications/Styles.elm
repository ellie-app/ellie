module Components.Notifications.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Notifications.Classes exposing (Classes(..))
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "components_notifications_")
        [ (.) Notifications
            [ position relative
            , displayFlex
            , alignItems center
            ]
        , (.) Button
            [ height (px (Constants.headerHeight - 10))
            , width (px (Constants.headerHeight - 10))
            , property "background" "none"
            , border zero
            , borderRadius (pct 50)
            , backgroundColor (hex "e7e7e7")
            , padding (px 10)
            , cursor pointer
            , position relative
            , boxShadow5 (px 0) (px 5) (px 9) (px -3) (rgba 0 0 0 0.5)
            , top (px -2)
            ]
        , (.) ButtonIcon
            [ position absolute
            , width (px 16)
            , height (px 16)
            , top (px 1)
            , left (px 28)
            ]
        , (.) Popout
            [ position absolute
            , backgroundColor (hex "f7f7f7")
            , width (px 400)
            , right (px (Constants.headerHeight / 2))
            , top (px (Constants.headerHeight + 24))
            , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
            , property "z-index" "5"
            , borderRadius (px 4)
            , borderTopRightRadius (px 0)
            , border3 (px 1) solid (hex "a7a7a7")
            , transforms [ scaleX 1, scaleY 1 ]
            , property "transition" "transform 0.1s ease-in-out, opacity 0.1s ease-in-out"
            , property "transform-origin" "top right"
            , before
                [ property "content" "''"
                , position absolute
                , borderBottom3 (px 16) solid (hex "f7f7f7")
                , borderLeft3 (px 16) solid transparent
                , width zero
                , height zero
                , top (px -16)
                , right zero
                , property "z-index" "2"
                ]
            , after
                [ property "content" "''"
                , position absolute
                , borderBottom3 (px 18) solid (hex "a7a7a7")
                , borderLeft3 (px 18) solid transparent
                , width zero
                , height zero
                , top (px -19)
                , right (px -1)
                ]
            ]
        , (.) PopoutHidden
            [ transforms [ scaleX 0, scaleY 0 ]
            , opacity (int 0)
            ]
        , (.) Items
            [ overflowY auto
            , maxHeight (px 500)
            , padding2 (px 8) zero
            ]
        , (.) Item
            [ padding2 (px 8) (px 16)
            , borderBottom3 (px 1) solid (hex "a7a7a7")
            , lastChild
                [ borderBottom zero
                ]
            ]
        , (.) ItemHighlighted
            [ backgroundColor (hex "e7e7e7")
            ]
        , (.) ItemDetails
            [ paddingBottom (px 8)
            , displayFlex
            , property "align-items" "center"
            ]
        , (.) ItemMessage
            [ property "width" "calc(100% - 24px)"
            , paddingRight (px 16)
            ]
        , (.) ItemIcon
            [ width (px 24)
            ]
        , (.) ItemTitle
            [ fontWeight (int 300)
            , fontSize (px 13)
            , textTransform uppercase
            , paddingTop (px 8)
            , paddingBottom (px 16)
            , displayFlex
            , property "justify-content" "space-between"
            ]
        , (.) ItemTimestamp
            [ color (hex "a7a7a7")
            ]
        , (.) Latest
            [ padding2 zero (px 16)
            , fontSize (px 14)
            , textTransform uppercase
            ]
        , (.) LatestTitle
            [ display inlineBlock
            , verticalAlign middle
            , lineHeight (int 1)
            ]
        , (.) LatestIcon
            [ width (px 16)
            , height (px 16)
            , display inlineBlock
            , marginRight (px 8)
            , verticalAlign middle
            ]
        ]
