module Data.Elm.Compiler.Module.Interface exposing (Interface, encoder, read)

import Data.Extra.Task as Task
import Data.FilePath as FilePath exposing (FilePath)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Make.FileStorage as FileStorage
import Task exposing (Task)


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
