module Data.Elm.Make.Config exposing (..)

import Data.FilePath as FilePath exposing (FilePath)


type alias Config =
    { artifactDirectory : FilePath
    , file : FilePath
    , debug : Bool
    , outputFilePath : FilePath
    }
