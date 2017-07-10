module Elm.Make.Location exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.FilePath as FilePath exposing (FilePath)
import Elm.Package as Package exposing (Package)


type alias Location =
    { relativePath : FilePath
    , package : Package
    }


decoder : Decoder Location
decoder =
    Decode.map2 Location
        (Decode.field "relativePath" Decode.string)
        (Decode.field "package" Package.decoder)


encoder : Location -> Value
encoder location =
    Encode.object
        [ ( "relativePath", Encode.string location.relativePath )
        , ( "package", Package.encoder location.package )
        ]
