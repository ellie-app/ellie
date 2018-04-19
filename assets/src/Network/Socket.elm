module Network.Socket
    exposing
        ( Info(..)
        , listen
        , send
        )

import Json.Decode as Decode exposing (Decoder, Value)
import WebSocket


type Info
    = Open
    | Data Value
    | Close


listen : String -> Sub Info
listen url =
    WebSocket.listen ("ELM_LANG_SOCKET::" ++ url) <|
        \input ->
            case Decode.decodeString infoDecoder input of
                Ok info ->
                    info

                Err _ ->
                    Close


send : String -> String -> Cmd msg
send url data =
    WebSocket.send ("ELM_LANG_SOCKET::" ++ url) data


infoDecoder : Decoder Info
infoDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "Open" ->
                        Decode.succeed Open

                    "Data" ->
                        Decode.map Data <| Decode.field "data" Decode.value

                    _ ->
                        Decode.succeed Close
            )
