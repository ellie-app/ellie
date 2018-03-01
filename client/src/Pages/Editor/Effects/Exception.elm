module Pages.Editor.Effects.Exception exposing (..)

import Extra.Json.Decode as Decode
import Http
import Json.Decode as Decode exposing (Decoder)


type Exception
    = PackageServerUnavailable
    | ClientNetworkError
    | ClientDecoderFailure String
    | Unknown String


decoder : Decoder Exception
decoder =
    Decode.oneOf
        [ Decode.genericUnion1 (\_ -> PackageServerUnavailable) "PackageServerUnavailable" (Decode.succeed ())
        , Decode.genericUnion1 Unknown "Unknown" Decode.string
        ]


fromString : String -> Exception
fromString string =
    case Decode.decodeString decoder string of
        Ok exception ->
            exception

        Err decoderError ->
            ClientDecoderFailure decoderError


fromHttp : Http.Error -> Exception
fromHttp error =
    case error of
        Http.BadUrl url ->
            ClientNetworkError

        Http.NetworkError ->
            ClientNetworkError

        Http.Timeout ->
            ClientNetworkError

        Http.BadStatus response ->
            fromString response.body

        Http.BadPayload innerError response ->
            ClientDecoderFailure innerError
