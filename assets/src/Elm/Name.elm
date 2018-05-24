module Elm.Name
    exposing
        ( Name
        , compare
        , decoder
        , encoder
        , fromString
        , toString
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Name =
    { user : String
    , project : String
    }


toString : Name -> String
toString packageName =
    packageName.user ++ "/" ++ packageName.project


fromString : String -> Result String Name
fromString input =
    case String.split "/" input of
        [ user, project ] ->
            Ok <| Name user project

        _ ->
            Err "Expecting a name like USER/PROJECT"


compare : Name -> Name -> Order
compare left right =
    Basics.compare (toString left) (toString right)


decoder : Decoder Name
decoder =
    Decode.andThen
        (\string ->
            case fromString string of
                Ok name ->
                    Decode.succeed name

                Err message ->
                    Decode.fail message
        )
        Decode.string


encoder : Name -> Value
encoder name =
    Encode.string (toString name)
