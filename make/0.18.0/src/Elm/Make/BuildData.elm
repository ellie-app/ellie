module Elm.Make.BuildData exposing (..)

import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Location as Location exposing (Location)


type alias BuildData =
    { blocking : List CanonicalModule
    , location : Location
    }
