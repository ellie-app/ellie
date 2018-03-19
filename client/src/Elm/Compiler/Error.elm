module Elm.Compiler.Error
    exposing
        ( Error
        , Location
        , Region
        , decoder
        , encoder
        , toLinterMessage
        )

import Ellie.Ui.CodeEditor as CodeEditor exposing (LinterMessage)
import Extra.Markdown as Markdown
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


toLinterMessage : Error -> LinterMessage
toLinterMessage error =
    { from = { line = error.region.start.line - 1, column = error.region.start.column - 1 }
    , to = { line = error.region.end.line - 1, column = error.region.end.column - 1 }
    , message = Markdown.toString <| error.message
    , severity =
        case error.level of
            "warning" ->
                CodeEditor.Warning

            _ ->
                CodeEditor.Error
    }


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
    , message : String
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
        |> Decode.required "message" Decode.string
        |> Decode.required "region" decodeRegion
        |> Decode.required "type" Decode.string


encoder : Error -> Value
encoder error =
    Encode.object
        [ ( "tag", Encode.string error.tag )
        , ( "message", Encode.string error.message )
        , ( "region", encodeRegion error.region )
        , ( "type", Encode.string error.level )
        ]
