module Elm.Compiler exposing (..)

import Data.HashDict as HashDict exposing (HashDict)
import Elm.Compiler.Error as Error exposing (Error, Location, Region)
import Elm.Compiler.Module as Module
import Elm.Compiler.Module.Interface exposing (Interface)
import Elm.Make.CanonicalModule exposing (CanonicalModule)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder, Value)
import Native.Compiler
import Task exposing (Task)


version : Version
version =
    Version 0 18 0


parseDependencies : Name -> String -> Task String (Result (List Error) ( Module.Raw, List Module.Raw ))
parseDependencies name source =
    parseDependencies_ name source
        |> Task.andThen handleResult


compile : Name -> Bool -> String -> HashDict CanonicalModule Interface -> Task String (Result (List Error) ( Interface, String ))
compile name isExposed source interfaces =
    interfaces
        |> HashDict.toList
        |> compile_ name isExposed source
        |> Task.andThen handleResult


handleResult : Result Value a -> Task String (Result (List Error) a)
handleResult result =
    case result of
        Ok output ->
            Task.succeed <| Ok output

        Err errorsValue ->
            case Decode.decodeValue (Decode.list Error.decoder) errorsValue of
                Ok errors ->
                    Task.succeed <| Err errors

                Err message ->
                    Task.fail <| "Couldn't understand compiler error: " ++ message


compile_ : Name -> Bool -> String -> List ( CanonicalModule, Interface ) -> Task String (Result Value ( Interface, String ))
compile_ =
    Native.Compiler.compile


parseDependencies_ : Name -> String -> Task String (Result Value ( Module.Raw, List Module.Raw ))
parseDependencies_ =
    Native.Compiler.parseDependencies
