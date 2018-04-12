module Pages.Editor.Effects.Exception exposing (..)

import Extra.Json.Decode as Decode
import Graphqelm.Http as Graphqelm
import Graphqelm.Http.GraphqlError as Graphqelm
import Http
import Json.Decode as Decode exposing (Decoder)


type Exception
    = PackageServerUnavailable
    | ClientNetworkError
    | ClientDecoderFailure String
    | GraphqlError (List Graphqelm.GraphqlError)
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


fromGqlError : Graphqelm.Error () -> Exception
fromGqlError error =
    case error of
        Graphqelm.GraphqlError _ errors ->
            GraphqlError errors

        Graphqelm.HttpError httpError ->
            fromHttp httpError


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
