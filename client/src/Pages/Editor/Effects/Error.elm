module Pages.Editor.Effects.Error exposing (..)

import Extra.String as String
import Http


type alias Error =
    String


fromHttp : Http.Error -> Error
fromHttp error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.NetworkError ->
            "Network Error"

        Http.Timeout ->
            "Request timed out"

        Http.BadStatus response ->
            "Request returned bad status: " ++ String.fromInt response.status.code

        Http.BadPayload innerError response ->
            "Client couldn't understand response: " ++ innerError
