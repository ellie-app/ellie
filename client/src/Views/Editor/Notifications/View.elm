module Views.Editor.Notifications.View
    exposing
        ( view
        )

import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (id, style)
import Shared.Colors as Colors
import Shared.Icons as Icons
import Views.Editor.Notifications.Classes exposing (..)


icon : Notification.Level -> Html msg
icon level =
    case level of
        Notification.Warning ->
            Icons.warning

        Notification.Error ->
            Icons.close

        Notification.Success ->
            Icons.checkmark

        Notification.Info ->
            Icons.information


iconColor : Notification.Level -> String
iconColor level =
    case level of
        Notification.Warning ->
            Colors.yellow

        Notification.Error ->
            Colors.red

        Notification.Success ->
            Colors.green

        Notification.Info ->
            Colors.blue


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


item : Notification -> Html msg
item notification =
    div [ class [ Item ] ]
        [ div [ class [ ItemTitle ] ]
            [ span [] [ text notification.title ]
            , span [ class [ ItemTimestamp ] ] [ text <| formatDate notification.timestamp ]
            ]
        , div [ class [ ItemDetails ] ]
            [ div [ class [ ItemMessage ] ] [ text notification.message ]
            , div
                [ class [ ItemIcon ]
                , style [ ( "color", iconColor notification.level ) ]
                ]
                [ icon notification.level
                ]
            ]
        ]


view : List Notification -> Html msg
view notifications =
    div
        [ class [ Popout ]
        , id "notifications"
        ]
        (List.map item notifications)
