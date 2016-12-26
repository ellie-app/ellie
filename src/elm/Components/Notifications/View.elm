module Components.Notifications.View
    exposing
        ( view
        , Context
        )

import Html exposing (Html, div, text, button, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Types.Notification as Notification exposing (Notification)
import Shared.Icons as Icons
import Components.Notifications.Classes exposing (..)


type alias Context msg =
    { notifications : List Notification
    , isOpen : Bool
    , onToggled : msg
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


iconColor : Notification.Level -> String
iconColor level =
    case level of
        Notification.Warning ->
            "#ded10c"

        Notification.Error ->
            "#db5555"

        Notification.Success ->
            "#55db61"

        Notification.Info ->
            "#55B5DB"


item : Notification -> Html msg
item notification =
    div [ class [ Item ] ]
        [ div [ class [ ItemDetails ] ]
            [ div [ class [ ItemTitle ] ] [ text notification.title ]
            , div [] [ text notification.message ]
            ]
        , div
            [ class [ ItemIcon ]
            , style [ ( "color", iconColor notification.level ) ]
            ]
            [ icon notification.level
            ]
        ]


popout : Context msg -> Html msg
popout context =
    case context.notifications of
        [] ->
            text ""

        _ ->
            div
                [ classList
                    [ ( Popout, True )
                    , ( PopoutHidden, not context.isOpen )
                    ]
                ]
                [ div [ class [ Items ] ]
                    (List.map item context.notifications)
                ]


latest : Context msg -> Html msg
latest context =
    case List.head context.notifications of
        Just notification ->
            div
                [ class [ Latest ]
                , style [ ( "color", iconColor notification.level ) ]
                ]
                [ span [ class [ LatestIcon ] ] [ icon notification.level ]
                , span [ class [ LatestTitle ] ] [ text notification.title ]
                ]

        Nothing ->
            text ""


view : Context msg -> Html msg
view context =
    div [ class [ Notifications ] ]
        [ latest context
        , button
            [ class [ Button ]
            , onClick context.onToggled
            ]
            [ Icons.bell ]
        , popout context
        ]
