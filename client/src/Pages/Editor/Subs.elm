port module Pages.Editor.Subs exposing (subscriptions)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Ellie.CodeMirror as CodeMirror
import Json.Decode as Decode exposing (Decoder, Value)
import Keyboard
import Pages.Editor.Layout.Subscriptions as Layout
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Save.Subscriptions as Save
import Pages.Editor.Save.Update as Save
import Pages.Editor.Update as Update exposing (Msg(..))


port windowUnloadedIn : (() -> msg) -> Sub msg


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
                        case Debug.log "stage" stage of
                            Compiling { total, complete } ->
                                if complete == 0 then
                                    SaveMsg <| Save.CompileStarted total
                                else
                                    NoOp

                            Success url ->
                                SaveMsg <| Save.CompileCompleted (Ok url)

                            FinishedWithErrors errors ->
                                SaveMsg <| Save.CompileCompleted (Err errors)

                            Failed message ->
                                SaveMsg <| Save.CompileAborted message

                            _ ->
                                NoOp
                    )
                |> Result.withDefault NoOp
    in
    compileForSaveIn parse


clearNotifications : Model -> Sub Msg
clearNotifications model =
    if not (List.isEmpty model.notifications) then
        Keyboard.ups
            (\keyCode ->
                if keyCode == 27 then
                    ClearAllNotifications
                else
                    NoOp
            )
    else
        Sub.none


codeMirror : Sub Msg
codeMirror =
    CodeMirror.subscriptions
        |> Sub.map
            (\result ->
                case result of
                    Ok inbound ->
                        case inbound of
                            CodeMirror.ValueChanged id value ->
                                case id of
                                    "elmEditor" ->
                                        ElmCodeChanged value

                                    "htmlEditor" ->
                                        HtmlCodeChanged value

                                    _ ->
                                        NoOp

                    Err exception ->
                        ReportException exception
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ online OnlineChanged
        , jsError IframeJsError
        , compilerMessages
        , compileForSave
        , keyCombos model
        , clearNotifications model
        , codeMirror
        , Sub.map SaveMsg Save.subscriptions
        , Sub.map LayoutMsg <| Layout.subscriptions model.layout
        ]
