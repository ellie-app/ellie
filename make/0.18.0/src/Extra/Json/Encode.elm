module Extra.Json.Encode exposing (..)

import Json.Encode as Encode exposing (Value)


objectWithType : String -> List ( String, Value ) -> Value
objectWithType tipe data =
    Encode.object <|
        [ ( "type", Encode.string tipe ) ]
            ++ data
