module Data.Uuid exposing
    ( Uuid
    , decoder
    , encoder
    , eq
    , fromString
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Uuid
    = Uuid String


fromString : String -> Uuid
fromString =
    Uuid


toString : Uuid -> String
toString (Uuid string) =
    string


eq : Uuid -> Uuid -> Bool
eq (Uuid left) (Uuid right) =
    left == right


decoder : Decoder Uuid
decoder =
    Decode.map Uuid Decode.string


encoder : Uuid -> Value
encoder uuid =
    Encode.string (toString uuid)
