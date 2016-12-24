module Types.CompileError
    exposing
        ( Location
        , Region
        , CompileError
        , decode
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Location =
    { line : Int
    , column : Int
    }


type alias Region =
    { start : Location
    , end : Location
    }


type alias CompileError =
    { tag : String
    , overview : String
    , details : String
    , subregion : Maybe Region
    , region : Region
    , level : String
    }


decodeLocation : Decoder Location
decodeLocation =
    Decode.succeed Location
        |> Decode.required "line" Decode.int
        |> Decode.required "column" Decode.int


decodeRegion : Decoder Region
decodeRegion =
    Decode.succeed Region
        |> Decode.required "start" decodeLocation
        |> Decode.required "end" decodeLocation


decode : Decoder CompileError
decode =
    Decode.succeed CompileError
        |> Decode.required "tag" Decode.string
        |> Decode.required "overview" Decode.string
        |> Decode.required "details" Decode.string
        |> Decode.optional "subregion" (Decode.map Just decodeRegion) Nothing
        |> Decode.required "region" decodeRegion
        |> Decode.required "type" Decode.string
