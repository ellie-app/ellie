port module Apps.Editor.Subs exposing (subscriptions)

import Window
import Mouse
import Keyboard
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Shared.MessageBus as MessageBus
import Apps.Editor.Model as Model exposing (Model, PopoutState(..))
import Apps.Editor.Update as Update exposing (Msg(..))


port windowUnloadedIn : (() -> msg) -> Sub msg


port windowMessageIn : (Value -> msg) -> Sub msg


port online : (Bool -> msg) -> Sub msg


port jsError : (String -> msg) -> Sub msg


port compilerMessagesIn : (Value -> msg) -> Sub msg


port compileForSaveIn : (Value -> msg) -> Sub msg


keyCombos : Model -> Sub Msg
keyCombos model =
    Sub.batch
        [ Keyboard.downs
            (\code ->
                if Model.canCompile model && KeyCombo.controlShift model.keyCombo && code == 13 then
                    CompileRequested
                else
                    NoOp
            )
        , Sub.map KeyComboMsg <| KeyCombo.subscriptions
        ]


compilerMessages : Sub Msg
compilerMessages =
    let
        parse value =
            Decode.decodeValue CompileStage.decoder value
                |> Result.map CompileStageChanged
                |> Result.withDefault NoOp
    in
        compilerMessagesIn parse


compileForSave : Sub Msg
compileForSave =
    let
        parse value =
            let
                _ =
                    Debug.log "v" value
            in
                Decode.decodeValue CompileStage.decoder value
                    |> Result.map
                        (\stage ->
                            case (Debug.log "stage" stage) of
                                Success url ->
                                    SaveCompileSucceeded (Ok url)

                                FinishedWithErrors errors ->
                                    SaveCompileSucceeded (Err errors)

                                Failed message ->
                                    SaveCompileFailed message

                                _ ->
                                    NoOp
                        )
                    |> Result.withDefault NoOp
    in
        compileForSaveIn parse


windowMessageDecoder : Model -> Decoder Msg
windowMessageDecoder model =
    (Decode.field "type" Decode.string)
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "mouseup" ->
                        if model.resultDragging then
                            Decode.succeed ResultDragEnded
                        else if model.editorDragging then
                            Decode.succeed EditorDragEnded
                        else
                            Decode.succeed NoOp

                    "mousemove" ->
                        Decode.decode Mouse.Position
                            |> Decode.required "x" Decode.int
                            |> Decode.required "y" Decode.int
                            |> Decode.map
                                (\position ->
                                    if model.resultDragging then
                                        ResultDragged position
                                    else if model.editorDragging then
                                        EditorDragged position
                                    else
                                        NoOp
                                )

                    _ ->
                        Decode.fail "unrecognized message"
            )


windowMessage : Model -> Sub Msg
windowMessage model =
    if model.resultDragging || model.editorDragging || model.popoutState == NotificationsOpen then
        windowMessageIn (Decode.decodeValue (windowMessageDecoder model) >> Result.withDefault NoOp)
    else
        Sub.none


windowSize : Model -> Sub Msg
windowSize model =
    Window.resizes WindowSizeChanged


editorDrags : Model -> Sub Msg
editorDrags model =
    if model.editorDragging then
        Sub.batch
            [ Mouse.moves EditorDragged
            , Mouse.ups (\_ -> EditorDragEnded)
            ]
    else
        Sub.none


resultDrags : Model -> Sub Msg
resultDrags model =
    if model.resultDragging then
        Sub.batch
            [ Mouse.moves ResultDragged
            , Mouse.ups (\_ -> ResultDragEnded)
            ]
    else
        Sub.none


closeSearch : Model -> Sub Msg
closeSearch model =
    if model.searchOpen then
        Keyboard.ups
            (\keyCode ->
                if keyCode == 27 then
                    ToggleSearch
                else
                    NoOp
            )
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ online OnlineChanged
        , MessageBus.notifications NotificationReceived
        , windowSize model
        , resultDrags model
        , editorDrags model
        , windowMessage model
        , closeSearch model
        , jsError IframeJsError
        , compilerMessages
        , compileForSave
        , keyCombos model
        ]
