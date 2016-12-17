module Components.PackageSearch.Model
    exposing
        ( Model(..)
        , model
        , updatePackagesQuery
        , receivePackageSearchResult
        , selectPackage
        , updateVersionsQuery
        , receiveVersionSearchResult
        , selectVersion
        , asVersions
        )

import RemoteData exposing (RemoteData(..))
import Types.Dependency as Dependency exposing (Dependency)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Version as Version exposing (Version)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Shared.Api as Api exposing (Error)


type Model
    = Packages (List PackageSearchResult) String
    | Versions PackageSearchResult (List Version) String
    | Ready Dependency


asPackages : Model -> Maybe ( List PackageSearchResult, String )
asPackages model =
    case model of
        Packages p s ->
            Just ( p, s )

        _ ->
            Nothing


asVersions : Model -> Maybe ( PackageSearchResult, List Version, String )
asVersions model =
    case model of
        Versions p v s ->
            Just ( p, v, s )

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
    Versions package [] ""


updateVersionsQuery : String -> Model -> Model
updateVersionsQuery query model =
    model
        |> asVersions
        |> Maybe.map (\( p, v, s ) -> Versions p v query)
        |> Maybe.withDefault model


receiveVersionSearchResult :
    String
    -> RemoteData Error (List Version)
    -> Model
    -> Model
receiveVersionSearchResult originalTerm data model =
    model
        |> asVersions
        |> Maybe.andThen (\( p, v, s ) -> pairToMaybe p (s == originalTerm))
        |> Maybe.map2 (\v p -> Versions p v originalTerm) (RemoteData.toMaybe data)
        |> Maybe.withDefault model


packageAndVersionToDep : PackageSearchResult -> Version -> Dependency
packageAndVersionToDep package version =
    Dependency
        package.username
        package.name
        (VersionRange version (Version.nextMajor version))


selectVersion : Version -> Model -> Model
selectVersion version model =
    model
        |> asVersions
        |> Maybe.map (\( p, _, _ ) -> packageAndVersionToDep p version)
        |> Maybe.map Ready
        |> Maybe.withDefault model
