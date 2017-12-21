module Pipeline.Install (getSolution) where


import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Map (lookup) as Map
import Data.Map.Extra (toArray) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Elm.Package (Package(..))
import Elm.Package as Package
import Elm.Package.Constraint (Constraint)
import Elm.Package.Constraint as Constraint
import Elm.Package.Description (Description(..))
import Elm.Package.Description as Description
import Elm.Package.Name (Name)
import Elm.Package.Version (Version)
import Pipeline.Install.Solver (Solution)
import Pipeline.Install.Solver as Solver
import System.FileSystem (FilePath)
import System.FileSystem as FileSystem


type PackageData =
  { sources :: Array (Tuple FilePath String)
  , artifacts :: Array (Tuple FilePath String)
  , description :: Description
  }


downloadPackage :: Package -> Task PackageData
downloadPackage package@(Package { name, version }) =
  { sources: _, artifacts: _, description: _ }
    <$> Package.fetchSources package
    <*> Package.fetchArtifacts package
    <*> Description.fetch name version
    |> lmap (\e -> (BM.PackageProblem <| "Failed to download " <> show package <> ": " <> show e))


saveFilesToStorage :: Package -> PackageData -> Task Unit
saveFilesToStorage package@(Package { name, version }) { sources, artifacts, description } =
    (const (const (const unit)))
      <$> Package.saveSources package sources
      <*> Package.saveArtifacts package artifacts
      <*> Description.save description
      >>= (\_ -> Package.markAsSaved package)
      |> lmap (const (BM.PackageProblem <| "Failed to save " <> show package))


downloadPackageIfNeeded :: Tuple Name Version -> Task Unit
downloadPackageIfNeeded (Tuple name version) = do
  let package = Package { name, version }
  alreadySaved <- Package.isSaved package
  if alreadySaved
    then
      pure unit
    else
      downloadPackage package
        >>= saveFilesToStorage package


install :: Description -> Task Solution
install (Description d) = do
  solution <- Solver.solve d.dependencies

  solution
    |> Map.toArray
    |> traverse downloadPackageIfNeeded
    |> map (const unit)

  FileSystem.write solutionPath solution

  pure solution


solutionPath :: FilePath
solutionPath =
  "/elm-stuff/exact-dependencies.json"


getSolution :: Description -> Task Solution
getSolution description@(Description d) = do
  savedSolutionExists <- FileSystem.exists solutionPath
  if savedSolutionExists
    then do
      solution <-
        FileSystem.read solutionPath
          |> lmap (const (BM.CorruptedArtifact solutionPath))

      if Array.all (solutionMatchesConstraint solution) d.dependencies
        then pure solution
        else install description
    else
      install description


solutionMatchesConstraint :: Solution -> Tuple Name Constraint -> Boolean
solutionMatchesConstraint solution (Tuple name constraint) =
    case Map.lookup name solution of
        Just version ->
            Constraint.isSatisfied constraint version

        Nothing ->
            false
