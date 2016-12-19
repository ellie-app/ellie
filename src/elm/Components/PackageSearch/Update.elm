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
    | VersionSelected Int


elmVersion : Version
elmVersion =
    Version 0 18 0


withNoCmd : Model -> ( Model, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


searchPackages : String -> Model -> ( Model, Cmd Msg )
searchPackages searchTerm model =
    ( model
    , Api.searchPackages elmVersion searchTerm
        |> Api.send (PackageSearchCompleted searchTerm)
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

        VersionSelected index ->
            model
                |> Model.updateVersionChoice index
                |> withNoCmd


initialize : Cmd Msg
initialize =
    Cmd.none
