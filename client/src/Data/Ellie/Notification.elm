module Data.Ellie.Notification
    exposing
        ( Level(..)
        , Notification
        , decoder
        , encoder
        , hash
        )

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Shared.Utils as Utils


type Level
    = Error
    | Warning
    | Success
    | Info


type alias Notification =
    { level : Level
    , message : String
    , title : String
    , timestamp : Date
    }


encodeLevel : Level -> Value
encodeLevel level =
    case level of
        Error ->
            Encode.object [ ( "tag", Encode.string "Level.Error" ) ]

        Warning ->
            Encode.object [ ( "tag", Encode.string "Level.Warning" ) ]

        Success ->
            Encode.object [ ( "tag", Encode.string "Level.Success" ) ]

        Info ->
            Encode.object [ ( "tag", Encode.string "Level.Info" ) ]


encoder : Notification -> Value
encoder notification =
    Encode.object
        [ ( "level", encodeLevel notification.level )
        , ( "message", Encode.string notification.message )
        , ( "title", Encode.string notification.title )
        , ( "timestamp", Encode.float (Date.toTime notification.timestamp) )
        ]


decodeLevel : Decoder Level
decodeLevel =
    Utils.decodeUnion "Level"
        [ ( "Error", Decode.succeed Error )
        , ( "Warning", Decode.succeed Warning )
        , ( "Success", Decode.succeed Success )
        , ( "Info", Decode.succeed Info )
        ]


decoder : Decoder Notification
decoder =
    Decode.decode Notification
        |> Decode.required "level" decodeLevel
        |> Decode.required "message" Decode.string
        |> Decode.required "title" Decode.string
        |> Decode.required "timestamp" (Decode.map Date.fromTime Decode.float)


hash : Notification -> String
hash notification =
    toString notification
