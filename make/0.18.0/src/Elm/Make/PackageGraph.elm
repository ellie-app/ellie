module Elm.Make.PackageGraph exposing (..)

import Dict exposing (Dict)
import Data.FilePath as FilePath exposing (FilePath)
import Elm.Package as Package exposing (Package)
import Elm.Compiler.Module as Module
import Elm.Make.PackageData as PackageData exposing (PackageData)


type alias PackageGraph =
    { packageData : Dict Module.Raw PackageData
    , packageNatives : Dict Module.Raw FilePath
    , packageForeignDependencies : Dict Module.Raw Package
    }
