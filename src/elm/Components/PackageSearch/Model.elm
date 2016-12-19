module Components.PackageSearch.Model
    exposing
        ( Model(..)
        , model
        , updatePackagesQuery
        , receivePackageSearchResult
        , selectPackage
        , asVersions
        , updateVersionChoice
        )

import RemoteData exposing (RemoteData(..))
import List.Nonempty as Nonempty exposing (Nonempty)
import Types.Dependency as Dependency exposing (Dependency)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Version as Version exposing (Version)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Shared.Api as Api exposing (Error)


type Model
    = Packages (List PackageSearchResult) String
    | Versions PackageSearchResult Version


asPackages : Model -> Maybe ( List PackageSearchResult, String )
asPackages model =
    case model of
        Packages p s ->
            Just ( p, s )

        _ ->
            Nothing


asVersions : Model -> Maybe ( PackageSearchResult, Version )
asVersions model =
    case model of
        Versions p v ->
            Just ( p, v )

        _ ->
            Nothing


boolToMaybe : Bool -> Maybe ()
boolToMaybe predicate =
    if predicate then
        Just ()
    else
        Nothing


pairToMaybe : a -> Bool -> Maybe a
pairToMaybe v p =
    if p then
        Just v
    else
        Nothing


model : Model
model =
    Packages [] ""


updatePackagesQuery : String -> Model -> Model
updatePackagesQuery query model =
    model
        |> asPackages
        |> Maybe.map (\( p, s ) -> Packages p query)
        |> Maybe.withDefault model


receivePackageSearchResult :
    String
    -> RemoteData Error (List PackageSearchResult)
    -> Model
    -> Model
receivePackageSearchResult originalTerm data model =
    model
        |> asPackages
        |> Maybe.andThen (\( p, s ) -> boolToMaybe (s == originalTerm))
        |> Maybe.andThen (\() -> RemoteData.toMaybe data)
        |> Maybe.map (\p -> Packages p originalTerm)
        |> Maybe.withDefault model


selectPackage : PackageSearchResult -> Model -> Model
selectPackage package model =
    Versions package (Nonempty.head package.versions)


updateVersionChoice : Int -> Model -> Model
updateVersionChoice index model =
    model
        |> asVersions
        |> Maybe.map (\( p, v ) -> Versions p (Nonempty.get index p.versions))
        |> Maybe.withDefault model


packageAndVersionToDep : PackageSearchResult -> Version -> Dependency
packageAndVersionToDep package version =
    Dependency
        package.username
        package.name
        (VersionRange version (Version.nextMajor version))
