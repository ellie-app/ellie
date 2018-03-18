module Elm.Name exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Name =
    { user : String
    , project : String
    }


toString : Name -> String
toString packageName =
    packageName.user ++ "/" ++ packageName.project


fromString : String -> Maybe Name
fromString input =
    case String.split "/" input of
        [ user, project ] ->
            Just <| Name user project

        _ ->
            Nothing


decoder : Decoder Name
decoder =
    Decode.andThen
        (\string ->
            case fromString string of
                Just version ->
                    Decode.succeed version

                Nothing ->
                    Decode.fail "No package name"
        )
        Decode.string


compare : Name -> Name -> Order
compare left right =
    Basics.compare (toString left) (toString right)


encoder : Name -> Value
encoder name =
    Encode.string (toString name)


virtualDom : Name
virtualDom =
    Name "elm-lang" "virtual-dom"
