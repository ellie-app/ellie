module Data.Elm.Make.Solution exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import EveryDict exposing (EveryDict)
import Data.Extra.EveryDict as EveryDict
import Data.Elm.Package.Version as Version exposing (Version)
import Data.Elm.Package.Name as Name exposing (Name)


type alias Solution =
    EveryDict Name Version


encoder : Solution -> Value
encoder solution =
    EveryDict.encoder Name.encoder Version.encoder solution


decoder : Decoder Solution
decoder =
    EveryDict.decoder Name.decoder Version.decoder
