module Data.FilePath exposing (FilePath, (</>), (<.>), resolve)

import Data.Stack as Stack exposing (Stack)


type alias FilePath =
    String


(</>) : FilePath -> FilePath -> FilePath
(</>) left right =
    let
        realLeft =
            if String.endsWith "/" left then
                String.dropRight 1 left
            else
                left

        realRight =
            if String.startsWith "/" right then
                String.dropLeft 1 right
            else
                right
    in
        realLeft ++ "/" ++ realRight


(<.>) : FilePath -> String -> FilePath
(<.>) path extension =
    path ++ "." ++ extension


loadStack : String -> Stack String -> Stack String
loadStack segment stack =
    case segment of
        "" ->
            stack

        "." ->
            stack

        ".." ->
            Tuple.second <| Stack.pop stack

        _ ->
            Stack.push segment stack


resolve : FilePath -> FilePath
resolve relative =
    relative
        |> String.split "/"
        |> List.foldl loadStack Stack.empty
        |> Stack.toList
        |> List.reverse
        |> String.join "/"
        |> (++) "/"
