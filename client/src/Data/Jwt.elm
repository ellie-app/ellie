module Data.Jwt exposing (Jwt, decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Jwt
    = Jwt String


toString : Jwt -> String
toString (Jwt token) =
    token


decoder : Decoder Jwt
decoder =
    Decode.map Jwt Decode.string
