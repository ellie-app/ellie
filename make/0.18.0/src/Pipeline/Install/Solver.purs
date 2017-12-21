module Pipeline.Install.Solver (solve, Solution) where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Task as Task
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Elm.Compiler.Version as Compiler
import Elm.Package.Constraint (Constraint)
import Elm.Package.Constraint as Constraint
import Elm.Package.Name (Name)
import Elm.Package.Version (Version)
import Pipeline.Install.Store as Store


type Solution =
  Map Name Version


type Packages =
  Map Name (Array Version)


solve :: Array (Tuple Name Constraint) -> Task Solution
solve constraints =
  constraints
    |> exploreConstraints
    >>= Task.fromMaybe (BM.PackageProblem "No solution for given constraints")

exploreConstraints :: Array (Tuple Name Constraint) -> Task (Maybe Solution)
exploreConstraints constraints =
    addConstraints Map.empty constraints
        |> map (Maybe.fromMaybe Map.empty)
        >>= explorePackages Map.empty


explorePackages :: Solution -> Packages -> Task (Maybe Solution)
explorePackages solution availablePackages =
    case Map.findMin availablePackages of
        Nothing ->
            pure (Just solution)

        Just { key: name, value: versions } ->
            exploreVersionList
              name
              versions
              solution
              (Map.delete name availablePackages)


exploreVersionList :: Name -> Array Version -> Solution -> Packages -> Task (Maybe Solution)
exploreVersionList name versions solution remainingPackages =
  versions
    |> Array.sort
    |> Array.reverse
    |> go
  where
    go vs =
      case Array.uncons vs of
          Nothing -> pure Nothing
          Just { head: version, tail } -> do
            maybeSolution <- exploreVersion name version solution remainingPackages
            case maybeSolution of
              Nothing -> go tail
              answer -> pure answer

exploreVersion :: Name -> Version -> Solution -> Packages -> Task (Maybe Solution)
exploreVersion name version solution remainingPackages = do
  (Tuple elmConstraint constraints) <-
    Store.getConstraints name version

  if Constraint.isSatisfied elmConstraint Compiler.version
    then
      let
        { yes: overlappingConstraints, no: newConstraints } =
          Array.partition (\(Tuple name _) -> Map.member name solution) constraints
      in
      if Array.all (satisfiedBy solution) overlappingConstraints
        then do
          maybePackages <- addConstraints remainingPackages newConstraints
          case maybePackages of
            Just extendedPackages ->
              explorePackages (Map.insert name version solution) extendedPackages

            Nothing ->
                pure Nothing
        else
          pure Nothing
    else
      pure Nothing


addConstraints :: Packages -> Array (Tuple Name Constraint) -> Task (Maybe Packages)
addConstraints packages constraints =
  case Array.uncons constraints of
    Nothing ->
      pure (Just packages)

    Just { head: (Tuple name constraint), tail } -> do
      versions <- Store.getVersions name
      case Array.filter (Constraint.isSatisfied constraint) versions of
        [] -> pure Nothing
        vs -> addConstraints (Map.insert name vs packages) tail


satisfiedBy :: Solution -> Tuple Name Constraint -> Boolean
satisfiedBy solution (Tuple name constraint) =
  case Map.lookup name solution of
    Nothing ->
      false

    Just version ->
      Constraint.isSatisfied constraint version
