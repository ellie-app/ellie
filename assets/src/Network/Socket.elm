port module Network.Socket exposing
    ( Info(..)
    , listen
    , send
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- INBOUND --


port networkSocketInbound : (Value -> msg) -> Sub msg


type Info
    = Open
    | Data Value
    | Close


listen : String -> Sub Info
listen url =
    networkSocketInbound <|
        \input ->
            case Decode.decodeValue infoDecoder input of
                Ok info ->
                    info

                Err _ ->
                    Close



-- OUTBOUND --


port networkSocketOutbound : Value -> Cmd msg


send : String -> String -> Cmd msg
send url data =
    networkSocketOutbound <|
        Encode.object
            [ ( "tag", Encode.string "Send" )
            , ( "url", Encode.string ("ELM_LANG_SOCKET::" ++ url) )
            , ( "data", Encode.string data )
            ]


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
