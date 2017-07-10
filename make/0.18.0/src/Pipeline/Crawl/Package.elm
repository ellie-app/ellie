module Pipeline.Crawl.Package exposing (dfsFromFiles, dfsFromExposedModules)

import Task exposing (Task)
import Dict exposing (Dict)
import Data.HashDict as HashDict exposing (HashDict)
import Json.Decode as Decode
import Extra.Dict as Dict
import Data.FilePath as FilePath exposing (FilePath, (</>), (<.>))
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Elm.Package.Description as Description exposing (Description)
import Elm.Package.Paths as Path
import Elm.Compiler.Module as Module
import Elm.Make.Solution as Solution exposing (Solution)
import Elm.Make.PackageGraph exposing (PackageGraph)
import Elm.Make.PackageData exposing (PackageData)
import Elm.Make.Error as BM
import Elm.Compiler as Compiler
import FileStorage as FileStorage

-- STATE and ENVIRONMENT


type alias Env =
    { sourceDirs : List FilePath
    , availableForeignModules : Dict Module.Raw (List (Name, Version))
    , allowNatives : Bool
    , packageName : Name
    }


second : (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)


initEnv : FilePath -> Description -> Solution -> Task BM.Error Env
initEnv root desc solution =
    readAvailableForeignModules desc solution
        |> Task.map
            (\availableForeignModules ->
                Env
                    (List.map ((</>) root) desc.sourceDirs)
                    availableForeignModules
                    desc.natives
                    desc.name
            )




-- GENERIC CRAWLER


dfsFromFiles
  : FilePath
  -> Solution
  -> Description
  -> List FilePath
  -> Task BM.Error (List Module.Raw, PackageGraph)
dfsFromFiles root solution desc filePaths =
    initEnv root desc solution
        |> Task.andThen
            (\env ->
                filePaths
                    |> List.map (readPackageData env Nothing)
                    |> Task.sequence
                    |> Task.andThen
                        (\info ->
                            let
                                names = List.map Tuple.first info
                                unvisited = List.concatMap (Tuple.second << Tuple.second) info
                                packageData = Dict.fromList (List.map (second Tuple.first) info)
                                initialGraph = PackageGraph packageData Dict.empty Dict.empty
                            in
                                dfs env unvisited initialGraph
                                    |> Task.map ((,) names)
                        )
            )


dfsFromExposedModules
  : FilePath
  -> Solution
  -> Description
  -> Task BM.Error PackageGraph
dfsFromExposedModules root solution desc =
    initEnv root desc solution
        |> Task.andThen
            (\env ->
                dfs
                    env
                    (List.map (Unvisited Nothing) desc.exposed)
                    (PackageGraph Dict.empty Dict.empty Dict.empty)
            )



-- DEPTH FIRST SEARCH


type alias Unvisited =
    { parent : Maybe Module.Raw
    , name : Module.Raw
    }


dfs : Env -> List Unvisited -> PackageGraph -> Task BM.Error PackageGraph
dfs env unvisited summary =
    case unvisited of
        [] ->
            Task.succeed summary

        next :: rest ->
            if Dict.member next.name summary.packageData then
                dfs env rest summary
            else
                dfsHelp env next rest summary


dfsHelp : Env -> Unvisited -> List Unvisited -> PackageGraph -> Task BM.Error PackageGraph
dfsHelp env { parent, name } unvisited summary =
    find env.allowNatives name env.sourceDirs
        |> Task.andThen
            (\filePaths ->
                case (filePaths, Dict.get name env.availableForeignModules) of
                    ([Elm filePath], Nothing) ->
                        readPackageData env (Just name) filePath
                            |> Task.andThen
                                (\(statedName, (packageData, newUnvisited)) ->
                                    dfs
                                        env
                                        (newUnvisited ++ unvisited)
                                        ({ summary | packageData = Dict.insert statedName packageData summary.packageData })
                                )

                    ([JavaScript filePath], Nothing) ->
                        dfs
                            env
                            unvisited
                            ({ summary | packageNatives = Dict.insert name filePath summary.packageNatives })

                    ([], Just [package]) ->
                        dfs
                            env
                            unvisited
                            ({ summary | packageForeignDependencies = Dict.insert name package summary.packageForeignDependencies })

                    ([], Nothing) ->
                        Task.fail <| BM.ModuleNotFound name parent

                    (_, maybePackages) ->
                        Task.fail <| BM.ModuleDuplicates
                            { name = name
                            , parent = parent
                            , local = List.map toFilePath filePaths
                            , foreign = maybePackages |> Maybe.map (List.map Tuple.first) |> Maybe.withDefault []
                            }
            )


-- FIND LOCAL FILE PATH


type CodePath
    = Elm FilePath
    | JavaScript FilePath


toFilePath : CodePath -> FilePath
toFilePath codePath =
    case codePath of
        Elm file -> file
        JavaScript file -> file


find : Bool -> Module.Raw -> List FilePath -> Task BM.Error (List CodePath)
find allowNatives moduleName sourceDirs =
    findHelp allowNatives [] moduleName sourceDirs

findHelp : Bool -> List CodePath -> Module.Raw -> List FilePath -> Task BM.Error (List CodePath)
findHelp allowNatives locations moduleName srcDirs =
    case srcDirs of
        [] ->
            Task.succeed locations

        dir :: srcDirs ->
            let
                consIf bool x xs =
                    if bool then
                        x :: xs
                    else
                        xs

                addElmPath locs =
                    let
                        elmPath =
                            dir </> Module.nameToPath moduleName <.> "elm"
                    in
                        FileStorage.exists elmPath
                            |> Task.mapError BM.PackageProblem
                            |> Task.map (\elmExists -> consIf elmExists (Elm elmPath) locs)

                addJsPath locs =
                    let
                        jsPath =
                            dir </> Module.nameToPath moduleName <.> "js"

                        jsExists =
                            case moduleName of
                                "Native" :: _ ->
                                    FileStorage.exists jsPath
                                        |> Task.mapError BM.PackageProblem
                                _ ->
                                    Task.succeed False
                    in
                        jsExists
                            |> Task.map (\exists -> consIf exists (JavaScript jsPath) locs)
            in
                addElmPath locations
                    |> Task.andThen
                        (\locations_ ->
                            if allowNatives then
                                addJsPath locations_
                            else
                                Task.succeed locations_
                        )
                    |> Task.andThen
                        (\updatedLocations ->
                            findHelp allowNatives updatedLocations moduleName srcDirs
                        )



-- READ and VALIDATE PACKAGE DATA for an ELM file


readPackageData
  : Env
  -> Maybe Module.Raw
  -> FilePath
  -> Task BM.Error (Module.Raw, (PackageData, List Unvisited))
readPackageData env maybeName filePath =
    FileStorage.read filePath
        |> Task.mapError BM.PackageProblem
        |> Task.map (Decode.decodeValue Decode.string >> Result.withDefault "")
        |> Task.andThen
            (\sourceCode ->
                Compiler.parseDependencies env.packageName sourceCode
                    |> Task.mapError (BM.CompilerErrors filePath sourceCode)
            )
        |> Task.andThen
            (\(name, deps) ->
                checkName filePath name maybeName
                    |> Task.map
                        (\_ ->
                            ( name
                            , ( PackageData filePath deps
                              , List.map (Unvisited (Just name)) deps
                              )
                            )
                        )
            )

checkName : FilePath -> Module.Raw -> Maybe Module.Raw -> Task BM.Error ()
checkName path nameFromSource maybeName =
    case maybeName of
        Just nameFromPath ->
            if nameFromPath /= nameFromSource then
                Task.fail <|
                    BM.ModuleName { path = path, actualName = nameFromSource, expectedName = nameFromPath }
            else
                Task.succeed ()
        _ ->
            Task.succeed ()


-- FOREIGN MODULES -- which ones are available, who exposes them?


readAvailableForeignModules
    : Description
    -> Solution
    -> Task BM.Error (Dict Module.Raw (List (Name, Version)))
readAvailableForeignModules desc solution =
    allVisible desc solution
        |> Task.andThen
            (\visiblePackages ->
                visiblePackages
                    |> List.map exposedModules
                    |> Task.sequence
            )
        |> Task.map (Dict.unionsWith (++))


allVisible
    : Description
    -> Solution
    -> Task BM.Error (List (Name, Version))
allVisible desc solution =
    let
        getVersion name =
            case HashDict.get name solution of
              Just version ->
                  Task.succeed (name, version)

              Nothing ->
                  Task.fail (BM.MissingPackage name)

        visible =
            List.map Tuple.first (desc.dependencies)
    in
        visible
            |> List.map getVersion
            |> Task.sequence


exposedModules
    : (Name, Version)
    -> Task BM.Error (Dict Module.Raw (List (Name, Version)))
exposedModules (name, version) =
    let
        insert moduleName dict =
            Dict.insert moduleName [(name, version)] dict
    in
        Description.read BM.PackageProblem (Path.package name version </> Path.description)
            |> Task.map (.exposed >> List.foldr insert Dict.empty)
