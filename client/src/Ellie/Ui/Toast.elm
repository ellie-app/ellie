module Ellie.Ui.Toast exposing (Config, view)

import Colors
import Css exposing (..)
import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Ellie.Ui.Icon as Icon
import Html.Styled exposing (Attribute, Html, fromUnstyled, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Markdown


type alias Config msg =
    { onClose : msg
    , notification : Notification
    }


view : Config msg -> Html msg
view { onClose, notification } =
    div [ containerStyles notification.level ]
        [ div [ itemTitleStyles ] [ text notification.title ]
        , div [ itemTimestampStyles ] [ text <| formatDate (Date.fromTime notification.timestamp) ]
        , div [ itemMessageStyles ] <| List.map fromUnstyled (Markdown.toHtml Nothing notification.message)
        , button
            [ closeButtonStyles
            , onClick onClose
            ]
            [ Icon.view Icon.Close ]
        ]


amPm : Date -> String
amPm date =
    if Date.hour date >= 12 then
        "PM"
    else
        "AM"


pad : Int -> String
pad num =
    if num >= 10 then
        toString num
    else
        "0" ++ toString num


formatDate : Date -> String
formatDate date =
    toString (Date.hour date % 12)
        ++ ":"
        ++ pad (Date.minute date)
        ++ ":"
        ++ pad (Date.second date)
        ++ " "
        ++ amPm date


-- STYLES


containerStyles : Notification.Level -> Attribute msg
containerStyles level =
    css
        [ backgroundColor Colors.darkGray
        , marginBottom (px 8)
        , padding (px 16)
        , position relative
        , color Colors.lightGray
        , property "align-items" "center"
        , Colors.boxShadow |> .popout
        , borderRadius (px 2)
        , lastChild [ marginBottom zero ]
        , borderLeft3 (px 2) solid Colors.green
        , case level of
            Notification.Warning ->
                borderColor Colors.yellow

            Notification.Error ->
                borderColor Colors.red

            Notification.Success ->
                borderColor Colors.green

            Notification.Info ->
                borderColor Colors.blue
        ]


itemMessageStyles : Attribute msg
itemMessageStyles =
    css
        [ property "white-space" "pre-wrap"
        , fontWeight normal
        , fontSize (px 16)
        , overflowX auto
        ]


itemTitleStyles : Attribute msg
itemTitleStyles =
    css
        [ fontSize (px 12)
        , textTransform uppercase
        , fontWeight bold
        ]


itemTimestampStyles : Attribute msg
itemTimestampStyles =
    css
        [ fontSize (px 12)
        , paddingBottom (px 12)
        ]


closeButtonStyles : Attribute msg
closeButtonStyles =
    css
        [ color Colors.mediumGray
        , property "background" "none"
        , position absolute
        , border zero
        , top (px 16)
        , right (px 16)
        , width (px 12)
        , height (px 12)
        , padding zero
        , cursor pointer
        , hover [ color Colors.lightMediumGray ]
        , active [ color Colors.lightGray ]
        ]
