module Data.Elm.Compiler.Module exposing (..)

import Data.FilePath as FilePath exposing ((</>), FilePath)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Raw =
    List String


nameToPath : Raw -> FilePath
nameToPath names =
    String.join "/" names


nameToString : Raw -> String
nameToString names =
    String.join "." names


hyphenate : Raw -> String
hyphenate names =
    String.join "-" names


nameDecoder : Decoder Raw
nameDecoder =
    Decode.list Decode.string


nameEncoder : Raw -> Value
nameEncoder raw =
    Encode.list <| List.map Encode.string raw
