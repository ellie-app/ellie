module Make.Elm.Compiler exposing (..)

import Task exposing (Task)
import EveryDict exposing (EveryDict)
import Json.Decode as Decode exposing (Decoder)
import Data.Extra.Task as Task
import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Package.Version as Version exposing (Version)
import Data.Elm.Compiler.Error as Error exposing (Error, Region, Location)
import Data.Elm.Compiler.Module as Module
import Data.Elm.Compiler.Module.Interface exposing (Interface)
import Data.Elm.Make.CanonicalModule exposing (CanonicalModule)
import Native.Compiler


version : Version
version =
    Version 0 18 0


parseDependencies : Name -> String -> Task (List Error) ( Module.Raw, List Module.Raw )
parseDependencies name source =
    Native.Compiler.parseDependencies name source


compile : Name -> Bool -> String -> EveryDict CanonicalModule Interface -> Task (List Error) ( Interface, String )
compile name isExposed source interfaces =
    Native.Compiler.compile name isExposed source (EveryDict.toList interfaces)
        |> Task.onError
            (\errorsValue ->
                Decode.decodeValue
                    (Decode.list Error.decoder)
                    errorsValue
                    |> Result.mapError
                        (\message ->
                            [ Error
                                "UNKNOWN"
                                "Compilation Failed"
                                ("Couldn't understand compiler error: " ++ message)
                                Nothing
                                (Region (Location 0 0) (Location 0 0))
                                "error"
                            ]
                        )
                    |> Task.fromResult
                    |> Task.andThen Task.fail
            )
