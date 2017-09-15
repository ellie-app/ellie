module Views.Editor.Notifications.View
    exposing
        ( Config
        , CssClasses(..)
        , namespace
        , view
        )

import Data.Ellie.Notification as Notification exposing (Notification)
import Date exposing (Date)
import Extra.Html as Html
import Extra.Html.Attributes exposing (style)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (id)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Shared.Colors as Colors
import Shared.Icons as Icons


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
        [ class [ Item ]
        , style "border-color" <| color notification.level
        ]
        [ div [ class [ ItemTitle ] ] [ text notification.title ]
        , div [ class [ ItemTimestamp ] ] [ text <| formatDate (Date.fromTime notification.timestamp) ]
        , div [ class [ ItemMessage ] ] [ text <| String.trim notification.message ]
        , div [ class [ ItemActions ] ]
            [ button
                [ class [ ItemActionButton, CloseButton ]
                , onClick (onClose notification)
                ]
                [ text "Dismiss" ]
            , case notification.action of
                Just action ->
                    button
                        [ class [ ItemActionButton ]
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
        [ class [ Notifications ]
        , id "notifications"
        ]
        (List.map (viewItem onClose onAction) notifications)


type CssClasses
    = Notifications
    | Item
    | ItemIcon
    | ItemDetails
    | Items
    | ItemTitle
    | ItemTimestamp
    | ItemMessage
    | ItemActions
    | ItemActionButton
    | CloseButton


namespace : String
namespace =
    "Views-Editor-Notifications-"


{ class } =
    Html.CssHelpers.withNamespace namespace
