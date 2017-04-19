port module Apps.Editor.Cmds
    exposing
        ( withCmd
        , withCmdWhen
        , withAdditionalCmd
        , compile
        , hasUnsavedWork
        , pathChanged
        , reloadIframe
        , openNewWindow
        , openDebugger
        )

import Json.Encode as Encode exposing (Value)
import Types.Package as Package exposing (Package)
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


port openNewWindow : String -> Cmd msg


port openDebuggerOut : () -> Cmd msg


port compileOnClientOut : ( String, String, Value ) -> Cmd msg


openDebugger : Cmd msg
openDebugger =
    openDebuggerOut ()


reloadIframe : Cmd msg
reloadIframe =
    reloadIframeOut ()


pathChanged : Cmd msg
pathChanged =
    pathChangedOut ()


compile : Model -> Cmd msg
compile model =
    compileOnClientOut
        ( model.stagedHtmlCode
        , model.stagedElmCode
        , Encode.list <| List.map Package.encode model.clientRevision.packages
        )
