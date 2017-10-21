module Pipeline.Install.Store where
  ( getConstraints
  , getVersions
  )

import Elm.Package (Name)
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Http


getVersions : ∀ e. Name -> Aff (ajax :: AJAX | e) (Array Version)
getVersions name =
    let
        path =
            Path.toVersionsList name
    in
    FileStorage.isOlderThan (10 * Time.minute) path
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
