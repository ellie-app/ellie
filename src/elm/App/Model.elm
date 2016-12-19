module App.Model
    exposing
        ( Model
        , model
        , acceptLoadedRevision
        , acceptSavedRevision
        , newRevision
        , updatedRevision
        , acceptSession
        , updateElmCode
        , updateHtmlCode
        , startCompiling
        , acceptCompileResult
        , hasChanges
        )

import Window
import RemoteData exposing (RemoteData(..))
import Types.NewRevision as NewRevision exposing (NewRevision)
import Types.ExistingRevision as ExistingRevision exposing (ExistingRevision)
import Types.Session as Session exposing (Session)
import Types.Dependency as Dependency exposing (Dependency)
import Types.CompileError as CompileError exposing (CompileError)
import Shared.Api as Api exposing (Error)
import Shared.Constants as Constants
import App.Routing as Routing exposing (Route(..))
import Components.Sidebar.Model as Sidebar


type alias Model =
    { session : RemoteData Error Session
    , compileResult : RemoteData Error (List CompileError)
    , htmlCode : String
    , elmCode : String
    , editorEditorSplit : Float
    , editorsResultSplit : Float
    , isDraggingEditorsSplit : Bool
    , isDraggingResultSplit : Bool
    , windowSize : Window.Size
    , sidebar : Sidebar.Model
    , title : String
    , description : String
    , currentRoute : Route
    , currentRevision : RemoteData Error ExistingRevision
    , dependencies : List Dependency
    }


model : Model
model =
    { session = Loading
    , elmCode = initElmCode
    , htmlCode = initHtmlCode
    , compileResult = NotAsked
    , editorEditorSplit = 0.5
    , editorsResultSplit = 0.5
    , isDraggingEditorsSplit = False
    , isDraggingResultSplit = False
    , windowSize = Window.Size 0 0
    , sidebar = Sidebar.model
    , title = "Untitled"
    , description = "Description..."
    , currentRoute = NotFound
    , currentRevision = NotAsked
    , dependencies = Constants.defaultDependencies
    }


hasChanges : Model -> Bool
hasChanges model =
    model.currentRevision
        |> RemoteData.map (\r -> r.elmCode /= model.elmCode || r.htmlCode /= model.htmlCode)
        |> RemoteData.withDefault False


acceptCompileResult : RemoteData Error (List CompileError) -> Model -> Model
acceptCompileResult data model =
    { model | compileResult = data }


startCompiling : Model -> Model
startCompiling model =
    case model.session of
        Success session ->
            { model | compileResult = Loading }

        _ ->
            model


updateElmCode : String -> Model -> Model
updateElmCode nextCode model =
    { model | elmCode = nextCode }


updateHtmlCode : String -> Model -> Model
updateHtmlCode nextCode model =
    { model | htmlCode = nextCode }


acceptSession : RemoteData Error Session -> Model -> Model
acceptSession data model =
    { model | session = data }


acceptLoadedRevision : RemoteData Error ExistingRevision -> Model -> Model
acceptLoadedRevision revision model =
    case revision of
        Success r ->
            { model
                | htmlCode = r.htmlCode
                , elmCode = r.elmCode
                , dependencies = r.dependencies
                , currentRevision = revision
            }

        _ ->
            { model | currentRevision = revision }


acceptSavedRevision : RemoteData Error ExistingRevision -> Model -> Model
acceptSavedRevision data model =
    { model | currentRevision = data }


newRevision : Model -> NewRevision
newRevision model =
    { htmlCode = model.htmlCode
    , elmCode = model.elmCode
    , dependencies = model.dependencies
    }


updatedRevision : Model -> Maybe ExistingRevision
updatedRevision model =
    model.currentRevision
        |> RemoteData.toMaybe
        |> Maybe.map
            (\r ->
                ExistingRevision
                    model.htmlCode
                    model.elmCode
                    model.dependencies
                    (r.revisionNumber + 1)
                    r.projectId
                    r.owned
            )


initHtmlCode : String
initHtmlCode =
    """<html>
  <head>
    <title></title>
    <meta charset="utf8" />
    <style>
        html, body {
          margin: 0;
          background: #F7F7F7;
          font-family: sans-serif;
        }

        main {
          color: red;
        }
    </style>
  </head>
  <body>
    <main></main>
    <script>
      var main = document.querySelector('main')
      var app = Elm.PortDemo.embed(main)
      setInterval(function () {
        app.ports.counter.send(1)
      }, 1000)
    </script>
  </body>
</html>
"""


initElmCode : String
initElmCode =
    """port module PortDemo exposing (main)

import Html exposing (text)


port counter : (Int -> msg) -> Sub msg


main =
    Html.program
        { view = \\count -> text <| toString count
        , update = \\next current -> (current + next, Cmd.none)
        , subscriptions = \\_ -> counter identity
        , init = (0, Cmd.none)
        }
"""
