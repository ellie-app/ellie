module Data.Jwt exposing (Jwt, decoder, encoder, toString, withTokenHeader)

import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Jwt
    = Jwt String


toString : Jwt -> String
toString (Jwt token) =
    token


encoder : Jwt -> Value
encoder (Jwt token) =
    Encode.string token


decoder : Decoder Jwt
decoder =
    Decode.map Jwt Decode.string


withTokenHeader : Jwt -> HttpBuilder.RequestBuilder a -> HttpBuilder.RequestBuilder a
withTokenHeader (Jwt token) builder =
    HttpBuilder.withBearerToken token builder
