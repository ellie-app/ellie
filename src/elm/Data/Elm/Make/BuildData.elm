module Data.Elm.Make.BuildData exposing (..)

import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Elm.Make.Location as Location exposing (Location)


type alias BuildData =
    { blocking : List CanonicalModule
    , location : Location
    }
