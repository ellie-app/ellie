module Data.Jwt exposing (Jwt, decoder, encoder, field, fromString, toString, withTokenHeader)

import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
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


field : SelectionSet String a -> SelectionSet Jwt a
field stringField =
    SelectionSet.map Jwt stringField
