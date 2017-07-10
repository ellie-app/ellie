module Elm.Compiler.Module.Interface exposing (Interface, read, encoder)

import Task exposing (Task)
import Extra.Task as Task
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.FilePath as FilePath exposing (FilePath)
import FileStorage as FileStorage


type Interface
    = Interface String


read : (String -> e) -> FilePath -> Task e Interface
read toError filepath =
    FileStorage.read filepath
        |> Task.map (Decode.decodeValue Decode.string)
        |> Task.andThen Task.fromResult
        |> Task.map Interface
        |> Task.mapError toError


encoder : Interface -> Value
encoder (Interface binary) =
    Encode.string binary
