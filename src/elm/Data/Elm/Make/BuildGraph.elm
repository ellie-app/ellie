module Data.Elm.Make.BuildGraph exposing (..)

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Elm.Make.BuildData as BuildData exposing (BuildData)


type alias BuildGraph =
    { blockedModules : EveryDict CanonicalModule BuildData
    , completedInterfaces : EveryDict CanonicalModule Interface
    }
