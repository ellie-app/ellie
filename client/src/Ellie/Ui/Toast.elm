module Ellie.Ui.Toast exposing (Config, view)

import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Toast.Styles as Styles
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)


type alias Config msg =
    { onClose : msg
    , notification : Notification
    }


view : Config msg -> Html msg
view { onClose, notification } =
    div
        [ Styles.container
        , colorClass notification.level
        ]
        [ div [ Styles.itemTitle ] [ text notification.title ]
        , div [ Styles.itemTimestamp ] [ text <| formatDate (Date.fromTime notification.timestamp) ]
        , div [ Styles.itemMessage ] [ text <| String.trim notification.message ]
        , button
            [ Styles.closeButton
            , onClick onClose
            ]
            [ Icon.view Icon.Close ]
        ]


colorClass : Notification.Level -> Attribute msg
colorClass level =
    case level of
        Notification.Warning ->
            Styles.warning

        Notification.Error ->
            Styles.error

        Notification.Success ->
            Styles.success

        Notification.Info ->
            Styles.info


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
