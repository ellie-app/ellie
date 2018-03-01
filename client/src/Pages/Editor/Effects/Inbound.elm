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
import Elm.Compiler.Error as CompilerError
import Elm.Package as Package exposing (Package)
import Extra.Result as Result
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keyboard
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.State exposing (Msg(..), State)
import WebSocket


type Inbound msg
    = CompileFinished Jwt (List CompilerError.Error -> msg)
    | OutputThrewException (String -> msg)
    | WorkspaceAttached Jwt (List Package -> msg)
    | KeepWorkspaceOpen Jwt
    | EscapePressed msg
    | WorkspaceDetached Jwt msg
    | Batch (List (Inbound msg))
    | None


listen : Inbound msg -> Sub (Msg msg)
listen inbound =
    case inbound of
        WorkspaceDetached token next ->
            Decode.string
                |> Decode.map
                    (\url ->
                        if url == (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token) then
                            UserMsg next
                        else
                            NoOp
                    )
                |> portDecoder "SocketClosed"
                |> pagesEditorEffectsInbound

        EscapePressed next ->
            Keyboard.ups
                (\keycode ->
                    if keycode == 27 then
                        UserMsg next
                    else
                        NoOp
                )

        KeepWorkspaceOpen token ->
            WebSocket.keepAlive (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        CompileFinished token callback ->
            Decode.list CompilerError.decoder
                |> Decode.map (callback >> UserMsg)
                |> socketDecoder "CompileFinished"
                |> WebSocket.listen (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        WorkspaceAttached token callback ->
            Decode.list Package.decoder
                |> Decode.map (callback >> UserMsg)
                |> socketDecoder "WorkspaceAttached"
                |> WebSocket.listen (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        OutputThrewException callback ->
            Decode.string
                |> Decode.map (callback >> UserMsg)
                |> portDecoder "OutputThrewException"
                |> pagesEditorEffectsInbound

        Batch inbounds ->
            Sub.batch <| List.map listen inbounds

        None ->
            Sub.none



--


socketDecoder : String -> Decoder (Msg msg) -> String -> Msg msg
socketDecoder topic decoder message =
    let
        messageDecoder =
            Decode.field "topic" Decode.string
                |> Decode.andThen
                    (\realTopic ->
                        if realTopic == topic then
                            Decode.oneOf
                                [ Decode.field "contents" decoder
                                , Decode.map ExceptionOccured <| Decode.field "exception" Exception.decoder
                                ]
                        else
                            Decode.succeed NoOp
                    )
    in
    message
        |> Decode.decodeString messageDecoder
        |> Result.fold identity (ClientDecoderFailure >> ExceptionOccured)


portDecoder : String -> Decoder (Msg msg) -> Value -> Msg msg
portDecoder topic decoder message =
    let
        decoder =
            Decode.field "topic" Decode.string
                |> Decode.andThen
                    (\realTopic ->
                        if realTopic == topic then
                            Decode.oneOf
                                [ Decode.map ExceptionOccured <| Decode.field "exception" Exception.decoder
                                , Decode.field "contents" decoder
                                ]
                        else
                            Decode.succeed NoOp
                    )
    in
    message
        |> Decode.decodeValue decoder
        |> Result.fold identity (ClientDecoderFailure >> ExceptionOccured)


port pagesEditorEffectsInbound : (Value -> msg) -> Sub msg


batch : List (Inbound msg) -> Inbound msg
batch =
    Batch


none : Inbound msg
none =
    None


wrapSubs :
    (model -> Inbound msg)
    -> ( model, State msg )
    -> Sub (Msg msg)
wrapSubs userSub ( model, state ) =
    listen <| userSub model


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        WorkspaceDetached token next ->
            WorkspaceDetached token (f next)

        EscapePressed next ->
            EscapePressed (f next)

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
