module Make.Pipeline.Crawl exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Json.Decode as Decode exposing (Decoder)
import Data.EverySet as EverySet exposing (EverySet)
import Data.Extra.List as List
import Data.Extra.EveryDict as EveryDict
import Data.Extra.Task as Task
import Data.FilePath as FilePath exposing (FilePath, (</>))
import Data.Elm.Make.Config as BM
import Data.Elm.Make.Error as BM
import Data.Elm.Package.Description as Description exposing (Description)
import Data.Elm.Package exposing (Package)
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Elm.Make.ProjectGraph as ProjectGraph exposing (ProjectGraph)
import Data.Elm.Make.ProjectData as ProjectData exposing (ProjectData)
import Data.Elm.Make.Location as Location exposing (Location)
import Data.Elm.Make.PackageGraph as PackageGraph exposing (PackageGraph)
import Data.Elm.Make.PackageData as PackageData exposing (PackageData)
import Data.Elm.Package.Paths as Path
import Data.Elm.Compiler.Module as Module
import Data.Elm.Make.Solution as Solution exposing (Solution)
import Make.Pipeline.Crawl.Package as CrawlPackage
import Make.FileStorage as FileStorage
import Make.Path as Path


type alias ProjectInfo =
    { package : Package
    , exposedModules : EverySet CanonicalModule
    , allModules : List CanonicalModule
    , graph : ProjectGraph Location
    }


crawl : BM.Config -> Description -> Solution -> Task BM.Error ProjectInfo
crawl config desc solution =
    EveryDict.toList solution
        |> List.map (crawlDependency config solution)
        |> Task.sequence
        |> Task.andThen
            (\depGraphs ->
                CrawlPackage.dfsFromFiles "." solution desc [ config.file ]
                    |> Task.andThen
                        (\( moduleForGeneration, packageGraph ) ->
                            let
                                thisPackage =
                                    ( desc.name, desc.version )

                                graph =
                                    canonicalizePackageGraph thisPackage packageGraph
                            in
                                case List.foldl1 union (graph :: depGraphs) of
                                    Just projectGraph ->
                                        Task.succeed <|
                                            ProjectInfo
                                                thisPackage
                                                (EverySet.fromList <| List.map (CanonicalModule thisPackage) desc.exposed)
                                                (List.map (CanonicalModule thisPackage) moduleForGeneration)
                                                projectGraph

                                    Nothing ->
                                        Task.fail <| BM.PackageProblem "Somehow got an empty package graph. This should be impossible."
                        )
            )


crawlDependency :
    BM.Config
    -> Solution
    -> Package
    -> Task BM.Error (ProjectGraph Location)
crawlDependency config solution ( name, version ) =
    let
        root =
            Path.package name version

        cache =
            Path.toPackageCacheFile config.artifactDirectory ( name, version )
    in
        FileStorage.read cache
            |> Task.map (Decode.decodeValue (ProjectGraph.decoder Location.decoder))
            |> Task.andThen Task.fromResult
            |> Task.onError
                (\_ ->
                    Description.read BM.PackageProblem (root </> Path.description)
                        |> Task.andThen (\description -> CrawlPackage.dfsFromExposedModules root solution description)
                        |> Task.map (canonicalizePackageGraph ( name, version ))
                        |> Task.andThen
                            (\projectGraph ->
                                FileStorage.write cache (ProjectGraph.encoder (Location.encoder) projectGraph)
                                    |> Task.mapError BM.PackageProblem
                                    |> Task.map (\_ -> projectGraph)
                            )
                )


canonicalizePackageGraph : Package -> PackageGraph -> ProjectGraph Location
canonicalizePackageGraph package { packageData, packageNatives, packageForeignDependencies } =
    let
        canonicalizeKeys =
            EveryDict.fromDict >> EveryDict.mapKeys (CanonicalModule package)
    in
        { projectData =
            EveryDict.map
                (\k v -> canonicalizePackageData package packageForeignDependencies v)
                (canonicalizeKeys packageData)
        , projectNatives =
            EveryDict.map
                (\k v -> Location v package)
                (canonicalizeKeys packageNatives)
        }


canonicalizePackageData :
    Package
    -> Dict Module.Raw Package
    -> PackageData
    -> ProjectData Location
canonicalizePackageData package foreignDependencies { packagePath, packageDependencies } =
    let
        canonicalizeModule moduleName =
            case Dict.get moduleName foreignDependencies of
                Nothing ->
                    CanonicalModule package moduleName

                Just foreignPackage ->
                    CanonicalModule foreignPackage moduleName
    in
        { projectLocation = Location packagePath package
        , projectDependencies = List.map canonicalizeModule packageDependencies
        }


union : ProjectGraph a -> ProjectGraph a -> ProjectGraph a
union left right =
    ProjectGraph
        (EveryDict.union left.projectData right.projectData)
        (EveryDict.union left.projectNatives right.projectNatives)
