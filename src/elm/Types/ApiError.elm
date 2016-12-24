module Types.ApiError
    exposing
        ( ApiError
        , encode
        )

import Json.Encode as Encode exposing (Value)
import Shared.Utils as Utils


type alias ApiError =
    { statusCode : Int
    , message : String
    , explanation : Maybe String
    }


encode : ApiError -> Value
encode apiError =
    Encode.object
        [ ( "statusCode", Encode.int apiError.statusCode )
        , ( "message", Encode.string apiError.message )
        , ( "explanation", Utils.encodeNullable Encode.string apiError.explanation )
        ]
