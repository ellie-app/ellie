module Pages.Editor.Layout.Model exposing (EditorCollapse(..), Model, init, reset)

import Window exposing (Size)


type EditorCollapse
    = BothOpen
    | JustHtmlOpen
    | JustElmOpen


type alias Model =
    { resultSplit : Float
    , resultDragging : Bool
    , editorSplit : Float
    , editorDragging : Bool
    , windowSize : Size
    , editorCollapse : EditorCollapse
    }


init : Size -> Model
init windowSize =
    { resultSplit = 0.5
    , resultDragging = False
    , editorSplit = 0.5
    , editorDragging = False
    , editorCollapse = BothOpen
    , windowSize = windowSize
    }


reset : Model -> Model
reset model =
    init model.windowSize


htmlIsHidden : Model -> Bool
htmlIsHidden model =
    model.editorCollapse == JustElmOpen


elmIsHidden : Model -> Bool
elmIsHidden model =
    model.editorCollapse == JustHtmlOpen
