module Data.Elm.Compiler.Error
    exposing
        ( Location
        , Region
        , Error
        , decoder
        , encoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type alias Location =
    { line : Int
    , column : Int
    }


type alias Region =
    { start : Location
    , end : Location
    }


type alias Error =
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


encodeLocation : Location -> Value
encodeLocation location =
    Encode.object
        [ ( "line", Encode.int location.line )
        , ( "column", Encode.int location.column )
        ]


decodeRegion : Decoder Region
decodeRegion =
    Decode.succeed Region
        |> Decode.required "start" decodeLocation
        |> Decode.required "end" decodeLocation


encodeRegion : Region -> Value
encodeRegion region =
    Encode.object
        [ ( "start", encodeLocation region.start )
        , ( "end", encodeLocation region.end )
        ]


decoder : Decoder Error
decoder =
    Decode.succeed Error
        |> Decode.required "tag" Decode.string
        |> Decode.required "overview" Decode.string
        |> Decode.required "details" Decode.string
        |> Decode.optional "subregion" (Decode.map Just decodeRegion) Nothing
        |> Decode.required "region" decodeRegion
        |> Decode.required "type" Decode.string


encoder : Error -> Value
encoder error =
    Encode.object
        [ ( "tag", Encode.string error.tag )
        , ( "overview", Encode.string error.overview )
        , ( "details", Encode.string error.details )
        , ( "subregion", error.subregion |> Maybe.map encodeRegion |> Maybe.withDefault Encode.null )
        , ( "region", encodeRegion error.region )
        , ( "type", Encode.string error.level )
        ]
