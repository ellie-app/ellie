module Views.Editor.Notifications.View
    exposing
        ( view
        )

import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Shared.Colors as Colors
import Shared.Icons as Icons
import Views.Editor.Notifications.Classes exposing (..)


type alias ViewModel msg =
    { onClose : Notification -> msg
    , notifications : List Notification
    }


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


color : Notification.Level -> String
color level =
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


item : (Notification -> msg) -> Notification -> Html msg
item onClose notification =
    div
        [ class [ Item ]
        , style [ ( "background-color", color notification.level ) ]
        ]
        [ div [ class [ ItemTitle ] ] [ text notification.title ]
        , div [ class [ ItemTimestamp ] ] [ text <| formatDate (Date.fromTime notification.timestamp) ]
        , div [ class [ ItemMessage ] ] [ text <| String.trim notification.message ]
        , button
            [ class [ CloseButton ]
            , onClick (onClose notification)
            ]
            [ Icons.closeEmpty
            ]
        ]


view : ViewModel msg -> Html msg
view { onClose, notifications } =
    div
        [ class [ Notifications ]
        , id "notifications"
        ]
        (List.map (item onClose) notifications)
