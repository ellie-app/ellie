module Components.PackageSearch.Update
    exposing
        ( Msg(..)
        , update
        , initialize
        )

import RemoteData exposing (RemoteData(..))
import Types.Version as Version exposing (Version)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Shared.Api as Api exposing (Error)
import Components.PackageSearch.Model as Model exposing (Model(..))


type Msg
    = NoOp
    | PackageQueryUpdated String
    | PackageSearchCompleted String (RemoteData Error (List PackageSearchResult))
    | PackageSelected PackageSearchResult
    | VersionQueryUpdated String
    | VersionSearchCompleted String (RemoteData Error (List Version))
    | VersionSelected Version


withNoCmd : Model -> ( Model, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


searchPackages : String -> Model -> ( Model, Cmd Msg )
searchPackages searchTerm model =
    ( model
    , Api.searchPackages searchTerm
        |> Api.send (PackageSearchCompleted searchTerm)
    )


searchVersions : String -> Model -> ( Model, Cmd Msg )
searchVersions searchTerm model =
    ( model
    , model
        |> Model.asVersions
        |> Maybe.map (\( p, _, _ ) -> p)
        |> Maybe.map (\p -> Api.searchVersions p.username p.name searchTerm)
        |> Maybe.map (Api.send (VersionSearchCompleted searchTerm))
        |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                |> withNoCmd

        PackageQueryUpdated query ->
            model
                |> Model.updatePackagesQuery query
                |> searchPackages query

        PackageSearchCompleted originalTerm data ->
            model
                |> Model.receivePackageSearchResult originalTerm data
                |> withNoCmd

        PackageSelected package ->
            model
                |> Model.selectPackage package
                |> withNoCmd

        VersionQueryUpdated query ->
            model
                |> Model.updateVersionsQuery query
                |> searchVersions query

        VersionSearchCompleted originalTerm data ->
            model
                |> Model.receiveVersionSearchResult originalTerm data
                |> withNoCmd

        VersionSelected version ->
            model
                |> Model.selectVersion version
                |> withNoCmd


initialize : Cmd Msg
initialize =
    Cmd.none
