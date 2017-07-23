module Pipeline.Install.Store exposing (getConstraints, getVersions)

import Constants as Constants
import Data.FilePath as FilePath exposing ((</>))
import Elm.Make.Constraint as Constraint exposing (Constraint)
import Elm.Package.Description as Description exposing (Description)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Paths as Path
import Elm.Package.Version as Version exposing (Version)
import Extra.Task as Task
import FileStorage as FileStorage
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Path as Path
import Task exposing (Task)
import Time exposing (Time)


getVersions : Name -> Task String (List Version)
getVersions name =
    let
        path =
            Path.toVersionsList name
    in
    FileStorage.isOlderThan (15 * Time.minute) path
        |> Task.andThen
            (\tooOld ->
                if not tooOld then
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
            "/package-artifacts"
                </> name.user
                </> name.project
                </> Version.toString version
                </> "elm-package.json"

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
                        (Constants.cdnBase ++ cdnKey)
                        Description.decoder
                        |> Http.toTask
                        |> Task.mapError (\_ -> "Couldn't download package description for " ++ Name.toString name)
            )
        |> Task.map (\desc -> ( desc.elmVersion, desc.dependencies ))
