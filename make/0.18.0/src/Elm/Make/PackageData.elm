module Elm.Make.PackageData exposing (..)

import Data.FilePath as FilePath exposing (FilePath)
import Elm.Compiler.Module as Module


type alias PackageData =
    { packagePath : FilePath
    , packageDependencies : List Module.Raw
    }
