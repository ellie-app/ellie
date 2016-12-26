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
            [ height (px Constants.headerHeight)
            , width (px Constants.headerHeight)
            , property "background" "none"
            , border zero
            , padding (px 10)
            , cursor pointer
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
                ]
            ]
        , (.) PopoutHidden
            [ transforms [ scaleX 0, scaleY 0 ]
            , opacity (int 0)
            ]
        , (.) Items
            [ overflowY auto
            , maxHeight (px 500)
            , padding2 (px 8) (px 16)
            ]
        , (.) Item
            [ displayFlex
            , padding2 (px 8) zero
            , borderBottom3 (px 1) solid (hex "a7a7a7")
            , lastChild
                [ borderBottom zero
                ]
            ]
        , (.) ItemDetails
            [ property "width" "calc(100% - 24px)"
            , paddingBottom (px 8)
            , paddingRight (px 16)
            ]
        , (.) ItemIcon
            [ width (px 24)
            ]
        , (.) ItemTitle
            [ fontWeight (int 300)
            , fontSize (px 13)
            , textTransform uppercase
            , padding2 (px 8) zero
            ]
        , (.) Latest
            [ padding2 zero (px 16)
            , fontSize (px 16)
            , textTransform uppercase
            ]
        , (.) LatestTitle
            [ display inlineBlock
            , verticalAlign middle
            , lineHeight (int 1)
            ]
        , (.) LatestIcon
            [ width (px 18)
            , height (px 18)
            , display inlineBlock
            , marginRight (px 8)
            , verticalAlign middle
            ]
        ]
