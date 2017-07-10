module Pipeline.Install exposing (..)

import Constants as Constants
import Data.FilePath as FilePath exposing ((</>), FilePath)
import Data.HashDict as HashDict exposing (HashDict)
import Elm.Make.Constraint as Constraint exposing (Constraint)
import Elm.Make.Error as BM
import Elm.Make.Solution as Solution exposing (Solution)
import Elm.Package as Package exposing (Package)
import Elm.Package.Description as Description exposing (Description)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Paths as Path
import Elm.Package.Version as Version exposing (Version)
import Extra.Task as Task
import FileStorage as FileStorage
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pipeline.Install.Solver as Solver
import Task exposing (Task)


downloadPackage : Package -> Task BM.Error ( List ( FilePath, String ), Description )
downloadPackage ( name, version ) =
    let
        cdnKey =
            name.user
                ++ "-"
                ++ name.project
                ++ "-"
                ++ Version.toString version
                ++ ".json"

        sourceUrl =
            Constants.cdnBase ++ "/package-artifacts/source-" ++ cdnKey

        descUrl =
            Constants.cdnBase ++ "/package-artifacts/package-" ++ cdnKey

        getSource =
            Http.get sourceUrl (Decode.keyValuePairs Decode.string)
                |> Http.toTask

        getDesc =
            Http.get descUrl Description.decoder
                |> Http.toTask
    in
    Task.map2 (,) getSource getDesc
        |> Task.mapError (Debug.log "e")
        |> Task.mapError (\_ -> BM.PackageProblem <| "Failed to download " ++ Name.toString name)


saveFilesToStorage : Package -> ( List ( FilePath, String ), Description ) -> Task BM.Error ()
saveFilesToStorage ( name, version ) ( files, description ) =
    let
        root =
            Path.package name version
    in
    files
        |> List.map
            (\( path, contents ) ->
                FileStorage.write
                    (root </> path)
                    (Encode.string contents)
            )
        |> Task.sequence
        |> Task.andThen
            (\_ ->
                Task.map2 (\_ _ -> ())
                    (FileStorage.write
                        (root </> Path.description)
                        (Description.encoder description)
                    )
                    (FileStorage.write
                        (root </> Path.downloadMarker)
                        (Encode.bool True)
                    )
            )
        |> Task.mapError BM.PackageProblem


packageAlreadySaved : Package -> Task BM.Error Bool
packageAlreadySaved ( name, version ) =
    FileStorage.exists (Path.package name version </> Path.downloadMarker)
        |> Task.mapError BM.PackageProblem


downloadPackageIfNeeded : Package -> Task BM.Error ()
downloadPackageIfNeeded package =
    packageAlreadySaved package
        |> Task.andThen
            (\alreadySaved ->
                if alreadySaved then
                    Task.succeed ()
                else
                    downloadPackage package
                        |> Task.andThen (saveFilesToStorage package)
            )


install : Description -> Task BM.Error Solution
install description =
    Solver.solve description.dependencies
        |> Task.mapError BM.PackageProblem
        |> Task.andThen
            (\solution ->
                solution
                    |> HashDict.toList
                    |> List.map downloadPackageIfNeeded
                    |> Task.sequence
                    |> Task.andThen
                        (\_ ->
                            FileStorage.write
                                Path.solvedDependencies
                                (Solution.encoder solution)
                                |> Task.mapError BM.PackageProblem
                                |> Task.map (\_ -> solution)
                        )
            )


getSolution : Description -> Task BM.Error Solution
getSolution description =
    FileStorage.read Path.solvedDependencies
        |> Task.map (Decode.decodeValue Solution.decoder)
        |> Task.andThen Task.fromResult
        |> Task.onError (\_ -> install description)
        |> Task.andThen
            (\solution ->
                if List.all (solutionMatchesConstraint solution) description.dependencies then
                    Task.succeed solution
                else
                    install description
            )


solutionMatchesConstraint : Solution -> ( Name, Constraint ) -> Bool
solutionMatchesConstraint solution ( name, constraint ) =
    case HashDict.get name solution of
        Just version ->
            Constraint.isSatisfied constraint version

        Nothing ->
            False
