module Data.Ellie.CompileStage
    exposing
        ( CompileStage(..)
        , decoder
        , encoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.Extra.Json.Encode as Encode
import Data.Elm.Compiler.Error as Error exposing (Error)


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
    | InstallingPackages
    | PlanningBuild
    | Compiling Int Int
    | GeneratingCode
    | Success String
    | FinishedWithErrors (List Error)
    | Failed String


encoder : CompileStage -> Value
encoder compileStage =
    case compileStage of
        Initial ->
            Encode.objectWithType "Initial" []

        LoadingCompiler percentage ->
            Encode.objectWithType "LoadingCompiler"
                [ ( "percentage", Encode.float percentage ) ]

        InstallingPackages ->
            Encode.objectWithType "InstallingPackages" []

        PlanningBuild ->
            Encode.objectWithType "PlanningBuild" []

        Compiling total complete ->
            Encode.objectWithType "Compiling"
                [ ( "total", Encode.int total )
                , ( "complete", Encode.int complete )
                ]

        GeneratingCode ->
            Encode.objectWithType "GeneratingCode" []

        Success url ->
            Encode.objectWithType "Success"
                [ ( "url", Encode.string url ) ]

        FinishedWithErrors errors ->
            Encode.objectWithType "FinishedWithErrors"
                [ ( "errors"
                  , Encode.list <| List.map Error.encoder errors
                  )
                ]

        Failed message ->
            Encode.objectWithType "Failed"
                [ ( "message", Encode.string message ) ]


decoder : Decoder CompileStage
decoder =
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

                    "InstallingPackages" ->
                        Decode.succeed InstallingPackages

                    "PlanningBuild" ->
                        Decode.succeed PlanningBuild

                    "Compiling" ->
                        Decode.map2
                            Compiling
                            (Decode.field "total" Decode.int)
                            (Decode.field "complete" Decode.int)

                    "GeneratingCode" ->
                        Decode.succeed GeneratingCode

                    "Success" ->
                        Decode.string
                            |> Decode.field "url"
                            |> Decode.map Success

                    "FinishedWithErrors" ->
                        Decode.list Error.decoder
                            |> Decode.field "errors"
                            |> Decode.map FinishedWithErrors

                    "Failed" ->
                        Decode.string
                            |> Decode.field "message"
                            |> Decode.map Failed

                    _ ->
                        Decode.fail (tipe ++ " is not a CompileStage constructor")
            )
