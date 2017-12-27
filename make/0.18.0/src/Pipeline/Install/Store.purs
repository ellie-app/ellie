module Pipeline.Install.Store where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (unwrap)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple(..))
import Data.Url as Url
import Elm.Package.Constraint (Constraint)
import Elm.Package.Description (Description(..))
import Elm.Package.Description as Description
import Elm.Package.Name (Name)
import Elm.Package.Version (Version)
import System.FileSystem ((</>))
import System.FileSystem as FileSystem
import System.Http as Http


getVersions :: Name -> Task (Array Version)
getVersions name = do
    let path = "/elm-stuff/packages" </> show name </> "versions.json"

    tooOld <-
      ifM (FileSystem.exists path)
        (path |> FileSystem.isOlderThan (Minutes 10.0) |> lmap (const (BM.CorruptedArtifact path)))
        (pure true)

    if not tooOld
      then
        path
          |> FileSystem.read
          |> lmap (const (BM.CorruptedArtifact path))
      else do
        versions <-
          Url.absolute [ "api/packages", show name, "versions" ] []
            |> Http.get
            |> Http.withHeader "Content-Type" "application/json"
            |> Http.withHeader "Accept" "application/json"
            |> Http.withExpect Http.expectJson
            |> Http.send
            |> lmap (const (BM.PackageProblem ("Could not load available versions for package " <> show name)))

        FileSystem.write path versions

        pure versions

type ConstraintSet
  = Tuple Constraint (Array (Tuple Name Constraint))

getConstraints :: Name -> Version -> Task ConstraintSet
getConstraints name version = do
  exists <- Description.isSaved name version

  if exists
    then
      Description.load name version
        |> lmap (unwrap >>> _.path >>> BM.CorruptedArtifact)
        |> rmap (\(Description d) -> Tuple d.elmVersion d.dependencies)
    else do
      description@(Description d) <-
        Description.fetch name version
          |> lmap (\error -> BM.PackageProblem ("Could not load elm-package.json for package " <> show name <> ": " <> show error))

      _ <- try <| Description.save name version description

      pure <| Tuple d.elmVersion d.dependencies
