module Views.Editor.Notifications exposing (Config, view)

import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Extra.Html as Html
import Extra.Html.Attributes exposing (style)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Shared.Colors as Colors
import Shared.Icons as Icons
import Views.Editor.Notifications.Styles as Styles


type alias Config msg =
    { onClose : Notification -> msg
    , notifications : List Notification
    , onAction : Notification.Action -> msg
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


viewItem : (Notification -> msg) -> (Notification.Action -> msg) -> Notification -> Html msg
viewItem onClose onAction notification =
    div
        [ Styles.item
        , style "border-color" <| color notification.level
        ]
        [ div [ Styles.itemTitle ] [ text notification.title ]
        , div [ Styles.itemTimestamp ] [ text <| formatDate (Date.fromTime notification.timestamp) ]
        , div [ Styles.itemMessage ] [ text <| String.trim notification.message ]
        , div [ Styles.itemActions ]
            [ button
                [ Styles.closeButton
                , onClick (onClose notification)
                ]
                [ text "Dismiss" ]
            , case notification.action of
                Just action ->
                    button
                        [ Styles.itemActionButton
                        , onClick <| onAction action
                        ]
                        [ text <| actionLabel action ]

                Nothing ->
                    Html.none
            ]
        ]


actionLabel : Notification.Action -> String
actionLabel action =
    case action of
        Notification.ClearElmStuff ->
            "Clear Cached Data"


view : Config msg -> Html msg
view { onClose, notifications, onAction } =
    div
        [ Styles.notifications
        , id "notifications"
        ]
        (List.map (viewItem onClose onAction) notifications)
