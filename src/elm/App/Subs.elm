port module App.Subs exposing (subscriptions)

import Mouse exposing (Position)
import Window
import Json.Decode as Decode exposing (Value)
import RemoteData exposing (RemoteData(..))
import App.Model as Model exposing (Model)
import App.Update as Update exposing (Msg(..))


port windowUnloadedIn : (() -> msg) -> Sub msg


port windowMessageIn : (Value -> msg) -> Sub msg


port online : (Bool -> msg) -> Sub msg



-- windowUnloaded : Model -> Sub Msg
-- windowUnloaded model =
--     model.session
--         |> RemoteData.map (\_ -> windowUnloadedIn (\_ -> WindowUnloaded))
--         |> RemoteData.withDefault Sub.none
--
--
-- windowMessage : Model -> Sub Msg
-- windowMessage model =
--     if model.isDraggingEditorsSplit || model.isDraggingResultSplit then
--         let
--             decoder =
--                 Decode.andThen
--                     (\type_ ->
--                         case type_ of
--                             "mouseup" ->
--                                 Decode.succeed StopDragging
--
--                             "mousemove" ->
--                                 Decode.map2 Position
--                                     (Decode.field "x" Decode.int)
--                                     (Decode.field "y" Decode.int)
--                                     |> Decode.map
--                                         (\position ->
--                                             if model.isDraggingResultSplit then
--                                                 ResultSplitDrags position
--                                             else if model.isDraggingEditorsSplit then
--                                                 EditorSplitDrags position
--                                             else
--                                                 NoOp
--                                         )
--
--                             _ ->
--                                 Decode.fail "unrecognized message"
--                     )
--                     (Decode.field "type" Decode.string)
--         in
--             windowMessageIn (Decode.decodeValue decoder >> Result.withDefault NoOp)
--     else
--         Sub.none
--
--
-- windowSize : Model -> Sub Msg
-- windowSize model =
--     Window.resizes WindowSizeChanged
--
--
-- editorDrags : Model -> Sub Msg
-- editorDrags model =
--     if model.isDraggingEditorsSplit then
--         Sub.batch
--             [ Mouse.moves EditorSplitDrags
--             , Mouse.ups (\_ -> StopDragging)
--             ]
--     else
--         Sub.none
--
--
-- resultDrags : Model -> Sub Msg
-- resultDrags model =
--     if model.isDraggingResultSplit then
--         Sub.batch
--             [ Mouse.moves ResultSplitDrags
--             , Mouse.ups (\_ -> StopDragging)
--             ]
--     else
--         Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ online OnlineChanged
        ]



-- Sub.batch
--     [ windowUnloaded model
--     , windowSize model
--     , editorDrags model
--     , resultDrags model
--     , windowMessage model
--     ]
