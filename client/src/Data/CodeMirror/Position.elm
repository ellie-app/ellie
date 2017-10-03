module Data.CodeMirror.Position exposing (Position, encoder)

import Json.Encode as Encode exposing (Value)


type alias Position =
    { line : Int
    , column : Int
    }


encoder : Position -> Value
encoder position =
    Encode.object
        [ ( "line", Encode.int position.line )
        , ( "ch", Encode.int position.column )
        , ( "sticky", Encode.null )
        ]
