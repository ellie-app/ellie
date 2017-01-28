module Components.Header.View
    exposing
        ( view
        , ViewModel
        , SaveOption(..)
        )

import Html exposing (Html, header, h1, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Components.Header.Classes exposing (..)
import Shared.Icons as Icons
import Shared.Utils as Utils exposing (renderIf)


type SaveOption
    = Save
    | Update
    | Fork
    | Saving


type alias ViewModel msg =
    { compileButtonEnabled : Bool
    , embedLinkButtonEnabled : Bool
    , saveButtonEnabled : Bool
    , saveButtonOption : SaveOption
    , buttonsVisible : Bool
    , onSave : msg
    , onCompile : msg
    , onFormat : msg
    , onAbout : msg
    , onEmbedLink : msg
    , onNotifications : msg
    , notificationCount : Int
    }


viewLogo : Html msg
viewLogo =
    div [ class [ Logo ] ]
        [ text "Ellie" ]


viewButton : msg -> Bool -> Html msg -> String -> Html msg
viewButton clickMsg isDisabled icon label =
    button
        [ class [ Button ]
        , onClick clickMsg
        , disabled isDisabled
        ]
        [ div [ class [ ButtonInner ] ]
            [ span [ class [ ButtonIcon ] ]
                [ icon ]
            , if String.length label == 0 then
                text ""
              else
                span [ class [ ButtonText ] ]
                    [ text label ]
            ]
        ]


viewSaveButton : ViewModel msg -> Html msg
viewSaveButton viewModel =
    case viewModel.saveButtonOption of
        Fork ->
            viewButton
                viewModel.onSave
                (not viewModel.saveButtonEnabled)
                Icons.forkRepo
                "Fork"

        Update ->
            viewButton
                viewModel.onSave
                (not viewModel.saveButtonEnabled)
                Icons.cloudOutline
                "Update"

        Save ->
            viewButton
                viewModel.onSave
                (not viewModel.saveButtonEnabled)
                Icons.cloudOutline
                "Save"

        Saving ->
            viewButton
                viewModel.onSave
                True
                Icons.cloudOutline
                "Saving..."


viewCompileButton : ViewModel msg -> Html msg
viewCompileButton viewModel =
    viewButton
        viewModel.onCompile
        (not viewModel.compileButtonEnabled)
        Icons.playOutline
        "Compile"


viewFormatButton : ViewModel msg -> Html msg
viewFormatButton viewModel =
    viewButton
        viewModel.onFormat
        False
        Icons.format
        "Format"


viewAboutButton : ViewModel msg -> Html msg
viewAboutButton viewModel =
    viewButton
        viewModel.onAbout
        False
        Icons.lightning
        "About"


viewNotificationsButton : ViewModel msg -> Html msg
viewNotificationsButton viewModel =
    viewButton
        viewModel.onNotifications
        False
        Icons.bell
        (if viewModel.notificationCount == 0 then
            ""
         else
            toString viewModel.notificationCount
        )


viewEmbedLinkButton : ViewModel msg -> Html msg
viewEmbedLinkButton viewModel =
    viewButton
        viewModel.onEmbedLink
        (not viewModel.embedLinkButtonEnabled)
        Icons.share
        "Share"


view : ViewModel msg -> Html msg
view viewModel =
    header [ class [ Header ] ]
        [ renderIf
            viewModel.buttonsVisible
            (\_ ->
                div [ class [ HeaderGroup ] ]
                    [ viewLogo
                    , viewCompileButton viewModel
                    , viewSaveButton viewModel
                    , viewFormatButton viewModel
                    , viewEmbedLinkButton viewModel
                    ]
            )
        , div [ class [ HeaderGroup ] ]
            [ viewAboutButton viewModel
            , viewNotificationsButton viewModel
            ]
        ]
