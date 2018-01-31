module Pages.Editor.Layout.Model exposing (DragTarget(..), EditorCollapse(..), Model, model, reset)

import Window exposing (Size)


type EditorCollapse
    = BothOpen
    | JustHtmlOpen
    | JustElmOpen


type DragTarget
    = EditorsHandle
    | LogsHandle
    | OutputHandle
    | NoTarget


type alias Model =
    { resultSplit : Float
    , editorSplit : Float
    , dragTarget : DragTarget
    , windowSize : Size
    , editorCollapse : EditorCollapse
    , logsCollapsed : Bool
    , logsSplit : Float
    }


model : Size -> Model
model windowSize =
    { resultSplit = 0.5
    , editorSplit = 0.7
    , editorCollapse = BothOpen
    , windowSize = windowSize
    , logsCollapsed = True
    , logsSplit = 0.5
    , dragTarget = NoTarget
    }


reset : Model -> Model
reset m =
    model m.windowSize


htmlIsHidden : Model -> Bool
htmlIsHidden model =
    model.editorCollapse == JustElmOpen


elmIsHidden : Model -> Bool
elmIsHidden model =
    model.editorCollapse == JustHtmlOpen
