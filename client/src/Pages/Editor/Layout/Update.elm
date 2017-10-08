module Pages.Editor.Layout.Update exposing (..)

import Ellie.Constants as Constants
import Mouse
import Pages.Editor.Layout.Model as Model exposing (Model)
import Window


type Msg
    = ResultDragStarted
    | ResultDragged Mouse.Position
    | ResultDragEnded
    | EditorDragStarted
    | EditorDragged Mouse.Position
    | EditorDragEnded
    | WindowSizeChanged Window.Size
    | ToggleEditorCollapse Model.EditorCollapse
    | NoOp


clamp : comparable -> comparable -> comparable -> comparable
clamp minimum maximum current =
    if current >= maximum then
        maximum
    else if current <= minimum then
        minimum
    else
        current


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditorDragStarted ->
            { model | editorDragging = True }

        EditorDragged position ->
            { model
                | editorSplit =
                    position
                        |> (\p -> toFloat (p.y - Constants.headerHeight))
                        |> (\h -> h / toFloat (model.windowSize.height - Constants.headerHeight))
                        |> clamp 0.2 0.8
            }

        EditorDragEnded ->
            { model | editorDragging = False }

        WindowSizeChanged size ->
            { model | windowSize = size }

        ResultDragStarted ->
            { model | resultDragging = True }

        ResultDragged position ->
            { model
                | resultSplit =
                    position
                        |> (\p -> toFloat (p.x - Constants.sidebarWidth))
                        |> (\w -> w / toFloat (model.windowSize.width - Constants.sidebarWidth))
                        |> clamp 0.2 0.8
            }

        ResultDragEnded ->
            { model | resultDragging = False }

        ToggleEditorCollapse collapse ->
            { model
                | editorCollapse =
                    if model.editorCollapse == collapse then
                        Model.BothOpen
                    else
                        collapse
            }

        NoOp ->
            model
