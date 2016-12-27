module Components.Notifications.View
    exposing
        ( view
        , Context
        )

import Date exposing (Date)
import Json.Decode as Decode
import Html exposing (Html, div, text, button, span)
import Html.Attributes exposing (style, id)
import Html.Events exposing (onClick, onWithOptions)
import Types.Notification as Notification exposing (Notification)
import Shared.Icons as Icons
import Components.Notifications.Classes exposing (..)


type alias Context msg =
    { notifications : List Notification
    , isOpen : Bool
    , onToggled : msg
    , isHighlighted : Bool
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


item : Bool -> Int -> Notification -> Html msg
item isHighlighted index notification =
    div
        [ classList
            [ ( Item, True )
            , ( ItemHighlighted, isHighlighted && index == 0 )
            ]
        ]
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
                , id "notifications"
                ]
                [ div [ class [ Items ] ]
                    (List.indexedMap (item context.isHighlighted) context.notifications)
                ]


latest : Context msg -> Html msg
latest context =
    case List.head context.notifications of
        Just notification ->
            div
                [ class [ Latest ]
                , style [ ( "color", iconColor notification.level ) ]
                ]
                [ span [ class [ LatestTitle ] ] [ text notification.title ]
                ]

        Nothing ->
            text ""


view : Context msg -> Html msg
view context =
    div [ class [ Notifications ] ]
        [ latest context
        , button
            [ class [ Button ]
            , onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed context.onToggled)
            ]
            [ Icons.bell
            , context.notifications
                |> List.head
                |> Maybe.map (.level)
                |> Maybe.map (\l -> div [ class [ ButtonIcon ], style [ ( "color", iconColor l ) ] ] [ icon l ])
                |> Maybe.withDefault (text "")
            ]
        , popout context
        ]
