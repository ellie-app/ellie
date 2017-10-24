module Pages.Editor.Layout.Update exposing (..)

import Ellie.Constants as Constants
import Mouse
import Pages.Editor.Layout.Model as Model exposing (DragTarget(..), Model)
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
    | ToggleLogsCollapse
    | LogsDragStarted
    | LogsDragged Mouse.Position
    | LogsDragEnded
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
        ToggleLogsCollapse ->
            { model | logsCollapsed = not model.logsCollapsed }

        EditorDragStarted ->
            { model | dragTarget = EditorsHandle }

        EditorDragged position ->
            { model
                | editorSplit =
                    position
                        |> (\p -> toFloat (p.y - Constants.headerHeight))
                        |> (\h -> h / toFloat (model.windowSize.height - Constants.headerHeight))
                        |> clamp 0.2 0.8
            }

        EditorDragEnded ->
            { model | dragTarget = NoTarget }

        LogsDragStarted ->
            { model | dragTarget = LogsHandle }

        LogsDragged position ->
            -- TODO: extract vertical dragging stuff to a helper function
            { model
                | logsSplit =
                    position
                        |> (\p -> toFloat (p.y - Constants.headerHeight))
                        |> (\h -> h / toFloat (model.windowSize.height - Constants.headerHeight))
                        |> clamp 0.5 0.8
            }

        LogsDragEnded ->
            { model | dragTarget = NoTarget }

        WindowSizeChanged size ->
            { model | windowSize = size }

        ResultDragStarted ->
            { model | dragTarget = OutputHandle }

        ResultDragged position ->
            { model
                | resultSplit =
                    position
                        |> (\p -> toFloat (p.x - Constants.sidebarWidth))
                        |> (\w -> w / toFloat (model.windowSize.width - Constants.sidebarWidth))
                        |> clamp 0.2 0.8
            }

        ResultDragEnded ->
            { model | dragTarget = NoTarget }

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
