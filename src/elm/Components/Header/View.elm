module Components.Header.View
    exposing
        ( Context
        , SaveOption(..)
        , view
        )

import Html exposing (Html, header, h1, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Types.Notification as Notification exposing (Notification)
import Shared.Icons as Icons
import Components.Header.Classes exposing (..)
import Components.Notifications.View as Notifications


when : Bool -> (() -> Html msg) -> Html msg
when pred html =
    if pred then
        html ()
    else
        text ""


viewButton : msg -> Bool -> String -> Html msg -> Html msg
viewButton msg enabled content icon =
    button
        [ class [ Button ]
        , disabled <| not enabled
        , onClick msg
        ]
        [ span [ class [ ButtonIcon ] ] [ icon ]
        , span [] [ text content ]
        ]


viewSaveButton : msg -> SaveOption -> Bool -> Html msg
viewSaveButton msg saveOption enabled =
    case saveOption of
        Save ->
            viewButton msg enabled "Save" Icons.cloudOutline

        Saving ->
            viewButton msg False "Saving" Icons.cloudOutline

        Update ->
            viewButton msg enabled "Update" Icons.cloudOutline

        Fork ->
            viewButton msg enabled "Fork" Icons.forkRepo


type SaveOption
    = Save
    | Update
    | Fork
    | Saving


type alias Context msg =
    { compileButtonEnabled : Bool
    , saveButtonEnabled : Bool
    , saveButtonOption : SaveOption
    , onSave : msg
    , onCompile : msg
    , onFormat : msg
    , buttonsVisible : Bool
    , notifications : List Notification
    , notificationsOpen : Bool
    , notificationsHighlight : Bool
    , onNotificationsToggled : msg
    }


notificationsContext : Context msg -> Notifications.Context msg
notificationsContext context =
    { notifications = context.notifications
    , isOpen = context.notificationsOpen
    , onToggled = context.onNotificationsToggled
    , isHighlighted = context.notificationsHighlight
    }


view : Context msg -> Html msg
view context =
    header [ class [ Header ] ]
        [ div [ class [ HeaderLeftStuff ] ]
            [ div [ class [ Logo ] ]
                [ h1 [ class [ LogoText ] ] [ text "Ellie" ]
                ]
            , when context.buttonsVisible <|
                \() ->
                    viewButton
                        context.onCompile
                        True
                        "Compile"
                        Icons.playOutline
            , when context.buttonsVisible <|
                \() ->
                    viewSaveButton
                        context.onSave
                        context.saveButtonOption
                        context.saveButtonEnabled
            , when context.buttonsVisible <|
                \() ->
                    viewButton
                        context.onFormat
                        True
                        "Format"
                        Icons.format
            ]
        , div [] [ Notifications.view <| notificationsContext context ]
        ]
