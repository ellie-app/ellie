port module Ellie.Effect.Inbound exposing (..)

import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Constants as Constants
import Ellie.Effect.Error exposing (Error)
import Elm.Compiler.Error as CompilerError
import Elm.Package as Package exposing (Package)
import Extra.Result as Result
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import WebSocket


port ellieEffectInbound : ({ tag : String, data : Value } -> msg) -> Sub msg


type Inbound msg
    = CompileFinished Jwt (List CompilerError.Error -> msg)
    | OutputThrewException (String -> msg)
    | WorkspaceAttached Jwt (List Package -> msg)
    | None


listen : (Error -> msg) -> msg -> Inbound msg -> Sub msg
listen onError noOp inbound =
    case inbound of
        CompileFinished token callback ->
            Sub.none

        -- WebSocket.listen
        --     (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
        --     (\data ->
        --         data
        --             |> Decode.decodeString (decodeEither Decode.string (Decode.list CompilerError.decoder))
        --             |> Result.andThen identity
        --             |> Result.fold callback (always noOp)
        --     )
        WorkspaceAttached token callback ->
            WebSocket.listen
                (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
                (\data ->
                    data
                        |> Debug.log "d"
                        |> Decode.decodeString (genericUnion1 identity "WorkspaceAttached" (Decode.list Package.decoder))
                        |> Result.fold callback onError
                )

        OutputThrewException callback ->
            ellieEffectInbound <|
                \{ tag, data } ->
                    case tag of
                        "OutputThrewException" ->
                            data
                                |> Decode.decodeValue Decode.string
                                |> Result.fold callback onError

                        _ ->
                            noOp

        None ->
            Sub.none


genericUnion1 : (a -> b) -> String -> Decoder a -> Decoder b
genericUnion1 constructor tag a =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\actualTag ->
                if actualTag == tag then
                    Decode.map constructor <| Decode.field "contents" a
                else
                    Decode.fail <| "Expected constructor \"" ++ tag ++ "\". Got \"" ++ actualTag ++ "\"."
            )


decodeEither : Decoder x -> Decoder a -> Decoder (Result x a)
decodeEither x a =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Left" ->
                        x |> Decode.index 0 |> Decode.field "contents" |> Decode.map Err

                    "Right" ->
                        a |> Decode.index 0 |> Decode.field "contents" |> Decode.map Ok

                    _ ->
                        Decode.fail <| "Expecting an Either, got " ++ tag
            )
