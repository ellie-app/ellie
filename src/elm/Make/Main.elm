port module Make.Main exposing (main)

import Task exposing (Task)
import Platform
import EveryDict exposing (EveryDict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Data.File as File exposing (File)
import Data.Elm.Package.Description as Description exposing (Description)
import Data.Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Data.Elm.Make.Solution as Solution exposing (Solution)
import Make.Pipeline.Install as Install
import Make.Pipeline.Crawl as Crawl exposing (ProjectInfo)
import Make.Pipeline.Plan as Plan
import Make.Pipeline.Compile as Compile
import Make.Pipeline.Generate as Generate
import Data.Elm.Make.Error as BM
import Data.Elm.Make.Config as BM
import Make.FileStorage as FileStorage
import Data.Elm.Make.BuildGraph as BuildGraph exposing (BuildGraph)
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Ellie.CompileStage as CompileStage exposing (CompileStage)


type Stage
    = Installing
    | Crawling Solution
    | Planning ProjectInfo
    | Compiling ProjectInfo BuildGraph Compile.Model
    | Generating


type Model
    = Loading Float
    | Waiting
    | Running Int Description String Stage


type Msg
    = LoadedMoreCode Float
    | StartCompile Int Description String
    | BuildFailure BM.Error
    | InstallSuccess Solution
    | CrawlSuccess ProjectInfo
    | PlanSuccess BuildGraph
    | CompileUpdated ( CanonicalModule, Interface )
    | GenerateSuccess (Maybe File)
    | NoOp


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


msgDecoder : Decoder Msg
msgDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "StartCompile" ->
                        Decode.succeed StartCompile
                            |> andMap (Decode.at [ "args", "0" ] Decode.int)
                            |> andMap (Decode.at [ "args", "1" ] Description.decoder)
                            |> andMap (Decode.at [ "args", "2" ] Decode.string)

                    "LoadedMoreCode" ->
                        Decode.succeed LoadedMoreCode
                            |> andMap (Decode.at [ "args", "0" ] Decode.float)

                    _ ->
                        Decode.fail ("Unknown message " ++ tipe)
            )


port msgsIn : (Value -> msg) -> Sub msg


port compileOut : ( Int, Value ) -> Cmd msg


port stageChangedOut : Value -> Cmd msg


stageChanged : CompileStage -> Cmd msg
stageChanged stage =
    stageChangedOut <| CompileStage.encoder stage


msgs : Sub Msg
msgs =
    msgsIn
        (\value ->
            case Decode.decodeValue msgDecoder value of
                Ok value ->
                    value

                Err err ->
                    Debug.crash err
        )


config : BM.Config
config =
    { artifactDirectory = "elm-stuff/build-artifacts"
    , file = "/src/Main.elm"
    , debug = False
    , outputFilePath = "/build/out.js"
    }


runOrFail : (a -> Msg) -> Result BM.Error a -> Msg
runOrFail onSuccess result =
    case result of
        Ok a ->
            onSuccess a

        Err x ->
            BuildFailure x


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedMoreCode percentage ->
            if percentage == 1 then
                ( Waiting, stageChanged CompileStage.Initial )
            else
                ( Loading percentage
                , stageChanged <| CompileStage.LoadingCompiler percentage
                )

        StartCompile buildNumber description source ->
            case model of
                Waiting ->
                    ( Running buildNumber description source Installing
                    , Cmd.batch
                        [ FileStorage.write "/src/Main.elm" (Encode.string source)
                            |> Task.mapError BM.PackageProblem
                            |> Task.andThen (\_ -> Install.getSolution description)
                            |> Task.attempt (runOrFail InstallSuccess)
                        , stageChanged CompileStage.InstallingPackages
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        InstallSuccess solution ->
            case model of
                Running buildNumber description source Installing ->
                    ( Running buildNumber description source (Crawling solution)
                    , Cmd.batch
                        [ Crawl.crawl config description solution
                            |> Task.attempt (runOrFail CrawlSuccess)
                        , stageChanged CompileStage.PlanningBuild
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        CrawlSuccess projectInfo ->
            case model of
                Running buildNumber description source (Crawling solution) ->
                    ( Running buildNumber description source (Planning projectInfo)
                    , Plan.planBuild config projectInfo.graph
                        |> Task.attempt (runOrFail PlanSuccess)
                    )

                _ ->
                    ( model, Cmd.none )

        PlanSuccess buildGraph ->
            case model of
                Running buildNumber description source (Planning projectInfo) ->
                    let
                        dependencies =
                            EveryDict.map (\k v -> v.projectDependencies) projectInfo.graph.projectData

                        initialModel =
                            Compile.init
                                config.artifactDirectory
                                projectInfo.exposedModules
                                projectInfo.allModules
                                dependencies
                                buildGraph

                        runningModel =
                            { initialModel
                                | readyList = []
                                , runningList = List.map Tuple.first initialModel.readyList
                            }
                    in
                        ( Running buildNumber description source (Compiling projectInfo buildGraph runningModel)
                        , Cmd.batch
                            [ initialModel.readyList
                                |> List.map
                                    (\stuff ->
                                        Compile.buildModule initialModel stuff
                                            |> Task.attempt (runOrFail CompileUpdated)
                                    )
                                |> Cmd.batch
                            , stageChanged <| CompileStage.Compiling runningModel.numTasks runningModel.completedTasks
                            ]
                        )

                _ ->
                    ( model, Cmd.none )

        CompileUpdated ( canonicalModule, interface ) ->
            case model of
                Running buildNumber description source (Compiling projectInfo buildGraph model) ->
                    let
                        _ =
                            Debug.log "cm" canonicalModule

                        updatedModel =
                            Compile.registerSuccess canonicalModule interface model

                        runningModel =
                            { updatedModel
                                | readyList = []
                                , runningList = List.map Tuple.first updatedModel.readyList
                            }
                    in
                        if updatedModel.completedTasks == updatedModel.numTasks then
                            ( Running buildNumber description source Generating
                            , Cmd.batch
                                [ Generate.generate
                                    config
                                    updatedModel.completedInterfaces
                                    updatedModel.dependencies
                                    projectInfo.graph.projectNatives
                                    projectInfo.allModules
                                    |> Task.attempt (runOrFail GenerateSuccess)
                                , stageChanged CompileStage.GeneratingCode
                                ]
                            )
                        else
                            ( Running buildNumber description source (Compiling projectInfo buildGraph runningModel)
                            , Cmd.batch
                                [ updatedModel.readyList
                                    |> List.map
                                        (\stuff ->
                                            Compile.buildModule updatedModel stuff
                                                |> Task.mapError (Debug.log (toString stuff))
                                                |> Task.attempt (runOrFail CompileUpdated)
                                        )
                                    |> Cmd.batch
                                , stageChanged <| CompileStage.Compiling runningModel.numTasks runningModel.completedTasks
                                ]
                            )

                _ ->
                    ( model, Cmd.none )

        GenerateSuccess result ->
            case model of
                Running buildNumber _ _ Generating ->
                    ( Waiting
                    , case result of
                        Just file ->
                            compileOut ( buildNumber, File.encoder file )

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BuildFailure error ->
            case model of
                Running _ _ _ _ ->
                    ( Waiting
                    , case error of
                        BM.CompilerErrors _ _ compilerErrors ->
                            stageChanged <| CompileStage.FinishedWithErrors compilerErrors

                        _ ->
                            stageChanged <| CompileStage.Failed (toString error)
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


main : Program Never Model Msg
main =
    Platform.program
        { init = ( Waiting, Cmd.none )
        , update = update
        , subscriptions = \_ -> msgs
        }
