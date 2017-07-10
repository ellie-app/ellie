module Data.Ellie.ApiError
    exposing
        ( ApiError
        , encoder
        )

import Json.Encode as Encode exposing (Value)


type alias ApiError =
    { statusCode : Int
    , message : String
    , explanation : String
    }


encoder : ApiError -> Value
encoder apiError =
    Encode.object
        [ ( "statusCode", Encode.int apiError.statusCode )
        , ( "message", Encode.string apiError.message )
        , ( "explanation", Encode.string apiError.explanation )
        ]
