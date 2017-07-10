module Pipeline.Install.Solver exposing (solve)

import Data.HashDict as HashDict exposing (HashDict)
import Elm.Compiler as Compile
import Elm.Make.Constraint as Constraint exposing (Constraint)
import Elm.Make.Solution exposing (Solution)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Pipeline.Install.Store as Store
import Task exposing (Task)


type alias Packages =
    HashDict Name (List Version)


solve : List ( Name, Constraint ) -> Task String Solution
solve constraints =
    exploreConstraints constraints
        |> Task.andThen
            (\maybeSolution ->
                case maybeSolution of
                    Just solution ->
                        Task.succeed solution

                    Nothing ->
                        Task.fail "No solution"
            )


exploreConstraints : List ( Name, Constraint ) -> Task String (Maybe Solution)
exploreConstraints constraints =
    addConstraints HashDict.empty constraints
        |> Task.map (Maybe.withDefault HashDict.empty)
        |> Task.andThen (explorePackages HashDict.empty)


explorePackages : Solution -> Packages -> Task String (Maybe Solution)
explorePackages solution availablePackages =
    case HashDict.viewWithKey availablePackages of
        Nothing ->
            Task.succeed (Just solution)

        Just ( ( name, versions ), remainingPackages ) ->
            exploreVersionList name versions solution remainingPackages


exploreVersionList : Name -> List Version -> Solution -> Packages -> Task String (Maybe Solution)
exploreVersionList name versions solution remainingPackages =
    let
        go versions =
            case versions of
                [] ->
                    Task.succeed Nothing

                version :: rest ->
                    exploreVersion name version solution remainingPackages
                        |> Task.andThen
                            (\maybeSolution ->
                                case maybeSolution of
                                    Nothing ->
                                        go rest

                                    answer ->
                                        Task.succeed answer
                            )
    in
    go (List.reverse (List.sortWith Version.compare versions))


exploreVersion : Name -> Version -> Solution -> Packages -> Task String (Maybe Solution)
exploreVersion name version solution remainingPackages =
    Store.getConstraints name version
        |> Task.andThen
            (\( elmConstraint, constraints ) ->
                if Constraint.isSatisfied elmConstraint Compile.version then
                    let
                        ( overlappingConstraints, newConstraints ) =
                            List.partition (\( name, _ ) -> HashDict.member name solution) constraints
                    in
                    if List.all (satisfiedBy solution) overlappingConstraints then
                        addConstraints remainingPackages newConstraints
                            |> Task.andThen
                                (\maybePackages ->
                                    case maybePackages of
                                        Just extendedPackages ->
                                            explorePackages (HashDict.insert name version solution) extendedPackages

                                        Nothing ->
                                            Task.succeed Nothing
                                )
                    else
                        Task.succeed Nothing
                else
                    Task.succeed Nothing
            )


addConstraints : Packages -> List ( Name, Constraint ) -> Task String (Maybe Packages)
addConstraints packages constraints =
    case constraints of
        [] ->
            Task.succeed (Just packages)

        ( name, constraint ) :: rest ->
            Store.getVersions name
                |> Task.andThen
                    (\versions ->
                        case List.filter (Constraint.isSatisfied constraint) versions of
                            [] ->
                                Task.succeed Nothing

                            vs ->
                                addConstraints (HashDict.insert name vs packages) rest
                    )


satisfiedBy : Solution -> ( Name, Constraint ) -> Bool
satisfiedBy solution ( name, constraint ) =
    case HashDict.get name solution of
        Nothing ->
            False

        Just version ->
            Constraint.isSatisfied constraint version
