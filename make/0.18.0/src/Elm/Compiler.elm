module Elm.Compiler exposing (..)

import Data.HashDict as HashDict exposing (HashDict)
import Elm.Compiler.Error as Error exposing (Error, Location, Region)
import Elm.Compiler.Module as Module
import Elm.Compiler.Module.Interface exposing (Interface)
import Elm.Make.CanonicalModule exposing (CanonicalModule)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Extra.Task as Task
import Json.Decode as Decode exposing (Decoder)
import Native.Compiler
import Task exposing (Task)


version : Version
version =
    Version 0 18 0


parseDependencies : Name -> String -> Task (List Error) ( Module.Raw, List Module.Raw )
parseDependencies name source =
    Native.Compiler.parseDependencies name source


compile : Name -> Bool -> String -> HashDict CanonicalModule Interface -> Task (List Error) ( Interface, String )
compile name isExposed source interfaces =
    Native.Compiler.compile name isExposed source (HashDict.toList interfaces)
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
