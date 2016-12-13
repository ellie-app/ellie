module App.Model exposing (Model, model)

import Window
import RemoteData exposing (RemoteData(..))
import Shared.Api as Api exposing (Error, CompileError, Session, Dependency, ExistingRevision)
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
    , dependencies = []
    , currentRevision = NotAsked
    }


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
