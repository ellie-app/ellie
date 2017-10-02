port module Pages.Editor.Cmds
    exposing
        ( clearElmStuff
        , compile
        , hasUnsavedWork
        , notify
        , openDebugger
        , openNewWindow
        , pathChanged
        , prepCompiler
        , reloadIframe
        , withAdditionalCmd
        , withCmd
        , withCmdWhen
        )

import Data.Ellie.Notification as Notification exposing (Notification)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Elm.Package.Constraint as Constraint exposing (Constraint)
import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Package.Version as Version exposing (Version)
import Json.Encode as Encode exposing (Value)
import Pages.Editor.Model as Model exposing (Model)
import Task
import Time


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


port prepCompilerOut : String -> Cmd msg


port clearElmStuffOut : () -> Cmd msg


clearElmStuff : Cmd msg
clearElmStuff =
    clearElmStuffOut ()


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


compileForSave : Model -> Cmd msg
compileForSave model =
    compileOnClientOut
        ( model.stagedHtmlCode
        , model.stagedElmCode
        , model.clientRevision
            |> Revision.toDescription
            |> .dependencies
            |> List.map (\( l, r ) -> ( Name.toString l, Constraint.encoder r ))
            |> Encode.object
        , True
        )


prepCompiler : Version -> Cmd msg
prepCompiler version =
    prepCompilerOut (Version.toString version)


type alias PartialNotification =
    { title : String
    , level : Notification.Level
    , message : String
    }


notify : (Notification -> msg) -> PartialNotification -> Cmd msg
notify tagger { title, level, message } =
    Time.now
        |> Task.map (Notification level message title)
        |> Task.perform tagger
