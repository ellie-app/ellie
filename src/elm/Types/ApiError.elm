module Types.ApiError
    exposing
        ( ApiError
        , encode
        )

import Json.Encode as Encode exposing (Value)


type alias ApiError =
    { statusCode : Int
    , message : String
    , explanation : String
    }


encode : ApiError -> Value
encode apiError =
    Encode.object
        [ ( "statusCode", Encode.int apiError.statusCode )
        , ( "message", Encode.string apiError.message )
        , ( "explanation", Encode.string apiError.explanation )
        ]
