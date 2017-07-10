module Elm.Make.BuildGraph exposing (..)

import Data.HashDict exposing (HashDict)
import Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Elm.Make.BuildData as BuildData exposing (BuildData)
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)


type alias BuildGraph =
    { blockedModules : HashDict CanonicalModule BuildData
    , completedInterfaces : HashDict CanonicalModule Interface
    }
