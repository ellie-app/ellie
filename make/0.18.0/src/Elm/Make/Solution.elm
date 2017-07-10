module Elm.Make.Solution exposing (..)

import Data.HashDict as HashDict exposing (HashDict)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Solution =
    HashDict Name Version


encoder : Solution -> Value
encoder solution =
    HashDict.encoder Name.encoder Version.encoder solution


decoder : Decoder Solution
decoder =
    HashDict.decoder Name.decoder Version.decoder
