module Data.Jwt exposing (Jwt, decoder, encoder, fromString, toString, withTokenHeader)

import Graphqelm.Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Jwt
    = Jwt String


toString : Jwt -> String
toString (Jwt token) =
    token


fromString : String -> Jwt
fromString =
    Jwt


encoder : Jwt -> Value
encoder (Jwt token) =
    Encode.string token


decoder : Decoder Jwt
decoder =
    Decode.map Jwt Decode.string


withTokenHeader : Jwt -> Graphqelm.Http.Request a -> Graphqelm.Http.Request a
withTokenHeader (Jwt token) =
    Graphqelm.Http.withHeader "authorization" ("Bearer " ++ token)
