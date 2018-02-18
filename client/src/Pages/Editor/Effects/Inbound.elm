port module Pages.Editor.Effects.Inbound
    exposing
        ( Inbound(..)
        , batch
        , map
        , none
        , wrapSubs
        )

import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Constants as Constants
import Ellie.Types.Log as Log exposing (Log)
import Elm.Compiler.Error as CompilerError
import Elm.Package as Package exposing (Package)
import Extra.Json.Decode as Decode
import Extra.Json.Encode as Encode
import Extra.Result as Result
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keyboard
import Pages.Editor.Effects.Error exposing (Error)
import Pages.Editor.Effects.State exposing (Msg(..), State)
import WebSocket


type Inbound msg
    = CompileFinished Jwt (List CompilerError.Error -> msg)
    | OutputThrewException (String -> msg)
    | WorkspaceAttached Jwt (List Package -> msg)
    | KeepWorkspaceOpen Jwt
    | LogReceived (Log -> msg)
    | Batch (List (Inbound msg))
    | EscapePressed msg
    | None


listen : (Error -> msg) -> Inbound msg -> Sub (Msg msg)
listen onError inbound =
    case inbound of
        EscapePressed next ->
            Keyboard.ups
                (\keycode ->
                    if keycode == 27 then
                        UserMsg next
                    else
                        NoOp
                )

        LogReceived callback ->
            pagesEditorEffectsInbound <|
                \{ tag, data } ->
                    case tag of
                        "LogReceived" ->
                            data
                                |> Decode.decodeValue Log.decoder
                                |> Result.fold callback onError
                                |> UserMsg

                        _ ->
                            NoOp

        KeepWorkspaceOpen token ->
            WebSocket.keepAlive (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        CompileFinished token callback ->
            WebSocket.listen
                (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
                (\data ->
                    data
                        |> Decode.decodeString
                            (Decode.genericUnion1
                                identity
                                "CompileSucceeded"
                                (Decode.list CompilerError.decoder)
                            )
                        |> Result.fold callback onError
                        |> UserMsg
                )

        WorkspaceAttached token callback ->
            WebSocket.listen
                (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
                (\data ->
                    data
                        |> Decode.decodeString (Decode.genericUnion1 identity "WorkspaceAttached" (Decode.list Package.decoder))
                        |> Result.fold callback onError
                        |> UserMsg
                )

        OutputThrewException callback ->
            pagesEditorEffectsInbound <|
                \{ tag, data } ->
                    case tag of
                        "OutputThrewException" ->
                            data
                                |> Decode.decodeValue Decode.string
                                |> Result.fold callback onError
                                |> UserMsg

                        _ ->
                            NoOp

        Batch inbounds ->
            Sub.batch <| List.map (listen onError) inbounds

        None ->
            Sub.none



--


port pagesEditorEffectsInbound : ({ tag : String, data : Value } -> msg) -> Sub msg


batch : List (Inbound msg) -> Inbound msg
batch =
    Batch


none : Inbound msg
none =
    None


wrapSubs :
    (Error -> msg)
    -> (model -> Inbound msg)
    -> ( model, State msg )
    -> Sub (Msg msg)
wrapSubs onError userSub ( model, state ) =
    listen onError <| userSub model


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        EscapePressed next ->
            EscapePressed (f next)

        LogReceived callback ->
            LogReceived (callback >> f)

        CompileFinished token callback ->
            CompileFinished token (callback >> f)

        OutputThrewException callback ->
            OutputThrewException (callback >> f)

        WorkspaceAttached token callback ->
            WorkspaceAttached token (callback >> f)

        KeepWorkspaceOpen token ->
            KeepWorkspaceOpen token

        Batch inbounds ->
            Batch <| List.map (map f) inbounds

        None ->
            None
