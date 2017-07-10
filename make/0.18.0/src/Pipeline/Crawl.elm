module Pipeline.Crawl exposing (..)

import Data.FilePath as FilePath exposing ((</>), FilePath)
import Data.HashDict as HashDict exposing (HashDict)
import Data.HashSet as HashSet exposing (HashSet)
import Dict exposing (Dict)
import Elm.Compiler.Module as Module
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Config as BM
import Elm.Make.Error as BM
import Elm.Make.Location as Location exposing (Location)
import Elm.Make.PackageData as PackageData exposing (PackageData)
import Elm.Make.PackageGraph as PackageGraph exposing (PackageGraph)
import Elm.Make.ProjectData as ProjectData exposing (ProjectData)
import Elm.Make.ProjectGraph as ProjectGraph exposing (ProjectGraph)
import Elm.Make.Solution as Solution exposing (Solution)
import Elm.Package exposing (Package)
import Elm.Package.Description as Description exposing (Description)
import Elm.Package.Paths as Path
import Extra.List as List
import Extra.Task as Task
import FileStorage as FileStorage
import Json.Decode as Decode exposing (Decoder)
import Path as Path
import Pipeline.Crawl.Package as CrawlPackage
import Task exposing (Task)


type alias ProjectInfo =
    { package : Package
    , exposedModules : HashSet CanonicalModule
    , allModules : List CanonicalModule
    , graph : ProjectGraph Location
    }


crawl : BM.Config -> Description -> Solution -> Task BM.Error ProjectInfo
crawl config desc solution =
    HashDict.toList solution
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
                                            (HashSet.fromList <| List.map (CanonicalModule thisPackage) desc.exposed)
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
                            FileStorage.write cache (ProjectGraph.encoder Location.encoder projectGraph)
                                |> Task.mapError BM.PackageProblem
                                |> Task.map (\_ -> projectGraph)
                        )
            )


canonicalizePackageGraph : Package -> PackageGraph -> ProjectGraph Location
canonicalizePackageGraph package { packageData, packageNatives, packageForeignDependencies } =
    let
        canonicalizeKeys =
            HashDict.fromDict >> HashDict.mapKeys (CanonicalModule package)
    in
    { projectData =
        HashDict.map
            (\k v -> canonicalizePackageData package packageForeignDependencies v)
            (canonicalizeKeys packageData)
    , projectNatives =
        HashDict.map
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
        (HashDict.union left.projectData right.projectData)
        (HashDict.union left.projectNatives right.projectNatives)
