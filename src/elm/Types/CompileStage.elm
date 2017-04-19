module Types.CompileStage
    exposing
        ( CompileStage(..)
        , decode
        )

import Json.Decode as Decode exposing (Decoder)
import Types.CompileError as CompileError exposing (CompileError)


{-| The stage of compilation in the Web Worker

Initial: the compilation hasn't started yet
LoadingCompiler: loading the big source script for the compiler from the CDN
    Float: what percentage of the script has been downloaded so far
Compiling: the compilation is proceding
    Int: how many modules have been discovered
    Int: how many modules have compiled successfully so far
Success: the compilation worked without problems
    String: the object url for the blob that contains the results of Compiling
            and embedding in the HTML
FinishedWithErrors: the compilation completed, but the compiler returned some
                    errors
Failed: the compiliation didn't finish
    String: an error message
-}
type CompileStage
    = Initial
    | LoadingCompiler Float
    | Compiling Int Int
    | Success String
    | FinishedWithErrors (List CompileError)
    | Failed String


decode : Decoder CompileStage
decode =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "Initial" ->
                        Decode.succeed Initial

                    "LoadingCompiler" ->
                        Decode.float
                            |> Decode.field "percentage"
                            |> Decode.map LoadingCompiler

                    "Compiling" ->
                        Decode.map2
                            Compiling
                            (Decode.field "total" Decode.int)
                            (Decode.field "complete" Decode.int)

                    "Success" ->
                        Decode.string
                            |> Decode.field "url"
                            |> Decode.map Success

                    "FinishedWithErrors" ->
                        Decode.list CompileError.decode
                            |> Decode.field "errors"
                            |> Decode.map FinishedWithErrors

                    "Failed" ->
                        Decode.string
                            |> Decode.field "message"
                            |> Decode.map Failed

                    _ ->
                        Decode.fail (tipe ++ " is not a CompileStage constructor")
            )
