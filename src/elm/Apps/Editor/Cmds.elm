port module Apps.Editor.Cmds
    exposing
        ( withCmd
        , withCmdWhen
        , withAdditionalCmd
        , compile
        , hasUnsavedWork
        , pathChanged
        , reloadIframe
        )

import Navigation
import RemoteData exposing (RemoteData(..))
import Types.CompileError as CompileError exposing (CompileError)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Notification as Notification exposing (Notification)
import Shared.MessageBus as MessageBus
import Shared.Api as Api
import Apps.Editor.Model as Model exposing (Model)


withCmd : (Model -> Cmd msg) -> Model -> ( Model, Cmd msg )
withCmd makeCmd model =
    ( model, makeCmd model )


withCmdWhen : (Model -> Bool) -> (Model -> Cmd msg) -> Model -> ( Model, Cmd msg )
withCmdWhen predicate makeCmd model =
    if predicate model then
        withCmd makeCmd model
    else
        ( model, Cmd.none )


withAdditionalCmd : (Model -> Cmd msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
withAdditionalCmd makeCmd ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , makeCmd model
        ]
    )


port hasUnsavedWork : Bool -> Cmd msg


port pathChangedOut : () -> Cmd msg


port reloadIframeOut : () -> Cmd msg


reloadIframe : Cmd msg
reloadIframe =
    reloadIframeOut ()


pathChanged : Cmd msg
pathChanged =
    pathChangedOut ()


compile : (Result ApiError (List CompileError) -> msg) -> Model -> Cmd msg
compile completeMsg model =
    let
        revision =
            model.clientRevision

        outRevision =
            { revision
                | elmCode = model.stagedElmCode
                , htmlCode = model.stagedHtmlCode
            }
    in
        Cmd.batch
            [ Api.compile outRevision
                |> Api.send completeMsg
            , MessageBus.notify
                Notification.Info
                "Compilation Started"
                "Ellie is compiling your code."
            ]
