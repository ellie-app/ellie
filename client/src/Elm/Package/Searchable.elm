module Elm.Package.Searchable exposing (..)

import Elm.Package as Package exposing (Package)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Searchable =
    { package : Package
    , description : String
    }


decoder : Decoder Searchable
decoder =
    Decode.decode Searchable
        |> Decode.required "package" Package.decoder
        |> Decode.required "description" Decode.string
