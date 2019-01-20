module Data.Jwt exposing (Jwt, decoder, encoder, field, fromString, toString, withTokenHeader)

import Graphql.Field
import Graphql.Http
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


withTokenHeader : Jwt -> Graphql.Http.Request a -> Graphql.Http.Request a
withTokenHeader (Jwt token) =
    Graphql.Http.withHeader "authorization" ("Bearer " ++ token)


field : Graphql.Field.Field String a -> Graphql.Field.Field Jwt a
field stringField =
    Graphql.Field.map Jwt stringField
