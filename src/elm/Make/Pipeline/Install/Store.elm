module Make.Pipeline.Install.Store exposing (getVersions, getConstraints)

import Task exposing (Task)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.FilePath as FilePath exposing ((</>))
import Data.Extra.Task as Task
import Data.Elm.Package.Version as Version exposing (Version)
import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Make.Constraint as Constraint exposing (Constraint)
import Data.Elm.Package.Paths as Path
import Data.Elm.Package.Description as Description exposing (Description)
import Make.FileStorage as FileStorage
import Make.Path as Path
import Shared.Constants as Constants


getVersions : Name -> Task String (List Version)
getVersions name =
    let
        path =
            Path.toVersionsList name
    in
        FileStorage.exists path
            |> Task.andThen
                (\exists ->
                    if exists then
                        FileStorage.read path
                            |> Task.map (Decode.decodeValue (Decode.list Version.decoder))
                            |> Task.andThen Task.fromResult
                    else
                        Http.get
                            (Constants.apiBase ++ "/packages/" ++ Name.toString name ++ "/versions")
                            (Decode.list Version.decoder)
                            |> Http.toTask
                            |> Task.mapError (\_ -> "Couldn't download tags for " ++ Name.toString name)
                            |> Task.andThen
                                (\tags ->
                                    FileStorage.write path (Encode.list <| List.map Version.encoder tags)
                                        |> Task.map (\_ -> tags)
                                )
                )


getConstraints : Name -> Version -> Task String ( Constraint, List ( Name, Constraint ) )
getConstraints name version =
    let
        cdnKey =
            "package-" ++ name.user ++ "-" ++ name.project ++ "-" ++ Version.toString version ++ ".json"

        path =
            Path.package name version </> Path.description
    in
        FileStorage.exists path
            |> Task.andThen
                (\exists ->
                    if exists then
                        FileStorage.read path
                            |> Task.map (Decode.decodeValue Description.decoder)
                            |> Task.andThen Task.fromResult
                    else
                        Http.get
                            ("https://cdn.ellie-app.com/package-artifacts/" ++ cdnKey)
                            (Description.decoder)
                            |> Http.toTask
                            |> Task.mapError (\_ -> "Couldn't download package description for " ++ Name.toString name)
                )
            |> Task.map (\desc -> ( desc.elmVersion, desc.dependencies ))
