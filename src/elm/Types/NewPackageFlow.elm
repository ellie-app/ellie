module Types.NewPackageFlow
    exposing
        ( NewPackageFlow(..)
        , updateSearchTerm
        , receiveResultsForTerm
        , selectVersionAtIndex
        , toDependency
        , encode
        , decode
        , hash
        )

import RemoteData exposing (RemoteData(..))
import List.Nonempty as Nonempty
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.ApiError as ApiError exposing (ApiError)
import Types.Version as Version exposing (Version)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Dependency as Dependency exposing (Dependency)
import Shared.Utils as Utils


type NewPackageFlow
    = NotSearching
    | PackageSearch String (List PackageSearchResult)
    | VersionSearch PackageSearchResult Version
    | Installation Dependency (RemoteData ApiError ())


updateSearchTerm : String -> NewPackageFlow -> NewPackageFlow
updateSearchTerm searchTerm flow =
    case flow of
        PackageSearch oldTerm oldResults ->
            PackageSearch searchTerm oldResults

        _ ->
            flow


receiveResultsForTerm : String -> List PackageSearchResult -> NewPackageFlow -> NewPackageFlow
receiveResultsForTerm searchTerm results flow =
    case flow of
        PackageSearch oldTerm oldResults ->
            if oldTerm == searchTerm then
                PackageSearch oldTerm results
            else
                flow

        _ ->
            flow


selectVersionAtIndex : Int -> NewPackageFlow -> NewPackageFlow
selectVersionAtIndex index flow =
    case flow of
        VersionSearch package version ->
            VersionSearch package (Nonempty.get index package.versions)

        _ ->
            flow


toDependency : NewPackageFlow -> Maybe Dependency
toDependency flow =
    case flow of
        VersionSearch package version ->
            Just <|
                Dependency
                    package.username
                    package.name
                    (VersionRange version (Version.nextMajor version))

        _ ->
            Nothing


beginInstall : NewPackageFlow -> NewPackageFlow
beginInstall flow =
    case flow of
        VersionSearch package version ->
            Installation
                (Dependency
                    package.username
                    package.name
                    (VersionRange version (Version.nextMajor version))
                )
                Loading

        _ ->
            flow


encode : NewPackageFlow -> Value
encode newPackageFlow =
    case newPackageFlow of
        NotSearching ->
            Encode.object
                [ ( "tag", Encode.string "NewPackageFlow.NotSearching" )
                ]

        PackageSearch searchTerm packages ->
            Encode.object
                [ ( "tag", Encode.string "NewPackageFlow.PackageSearch" )
                , ( "searchTerm", Encode.string searchTerm )
                , ( "packages", Encode.list <| List.map PackageSearchResult.encode packages )
                ]

        VersionSearch package version ->
            Encode.object
                [ ( "tag", Encode.string "NewPackageFlow.VersionSearch" )
                , ( "package", PackageSearchResult.encode package )
                , ( "version", Version.encode version )
                ]

        Installation dependency _ ->
            Encode.object
                [ ( "tag", Encode.string "NewPackageFlow.Installing" )
                , ( "dependency", Dependency.encode dependency )
                ]


decode : Decoder NewPackageFlow
decode =
    Utils.decodeUnion "NewPackageFlow"
        [ ( "NotSearching"
          , Decode.succeed NotSearching
          )
        , ( "PackageSearch"
          , Decode.decode PackageSearch
                |> Decode.required "searchTerm" Decode.string
                |> Decode.required "packages" (Decode.list PackageSearchResult.decode)
          )
        , ( "VersionSearch"
          , Decode.decode VersionSearch
                |> Decode.required "package" PackageSearchResult.decode
                |> Decode.required "version" Version.decode
          )
        , ( "Installation"
          , Decode.decode Installation
                |> Decode.required "dependency" Dependency.decode
                |> Decode.hardcoded NotAsked
          )
        ]


hash : NewPackageFlow -> String
hash newPackageFlow =
    toString newPackageFlow
