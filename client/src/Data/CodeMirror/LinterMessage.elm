module Data.CodeMirror.LinterMessage exposing (LinterMessage, Severity(..), encoder)

import Data.CodeMirror.Position as Position exposing (Position)
import Json.Encode as Encode exposing (Value)


type Severity
    = Error
    | Warning


type alias LinterMessage =
    { severity : Severity
    , message : String
    , from : Position
    , to : Position
    }


encoder : LinterMessage -> Value
encoder linterMessage =
    Encode.object
        [ ( "severity", severityEncoder linterMessage.severity )
        , ( "message", Encode.string linterMessage.message )
        , ( "from", Position.encoder linterMessage.from )
        , ( "to", Position.encoder linterMessage.to )
        ]


severityEncoder : Severity -> Value
severityEncoder severity =
    case severity of
        Error ->
            Encode.string "error"

        Warning ->
            Encode.string "warning"
