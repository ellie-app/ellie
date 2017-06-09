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
import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Make.Constraint as Constraint exposing (Constraint)
import Apps.Editor.Model as Model exposing (Model)
import Data.Ellie.Revision as Revision exposing (Revision)


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


port compileOnClientOut : ( String, String, Value, Bool ) -> Cmd msg


openDebugger : Cmd msg
openDebugger =
    openDebuggerOut ()


reloadIframe : Cmd msg
reloadIframe =
    reloadIframeOut ()


pathChanged : Cmd msg
pathChanged =
    pathChangedOut ()


compile : Model -> Bool -> Cmd msg
compile model forSave =
    compileOnClientOut
        ( model.stagedHtmlCode
        , model.stagedElmCode
        , model.clientRevision
            |> Revision.toDescription
            |> .dependencies
            |> List.map (\( l, r ) -> ( Name.toString l, Constraint.encoder r ))
            |> Encode.object
        , forSave
        )
