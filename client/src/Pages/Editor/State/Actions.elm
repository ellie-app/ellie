module Pages.Editor.State.Actions exposing (..)

import Elm.Package as Package exposing (Package)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)


type alias PackagesModel =
    { query : String
    , searchedPackages : Maybe (List Package)
    , awaitingSearch : Bool
    }


packages : Model
packages =
    Packages
        { query = ""
        , searchedPackages = Nothing
        , awaitingSearch = False
        }


type Model
    = Hidden
    | Packages PackagesModel
    | Settings
    | Help


type Msg
    = UserTypedInPackageSearch String
    | SearchedPackagesReceived (Maybe (List Package))


update : Msg -> Model -> ( Model, Outbound Msg )
update msg model =
    case ( model, msg ) of
        ( Packages packagesModel, UserTypedInPackageSearch query ) ->
            if String.isEmpty query then
                ( Packages { packagesModel | query = query, searchedPackages = Nothing, awaitingSearch = False }
                , Outbound.none
                )
            else if String.length query < 4 then
                ( Packages { packagesModel | query = query }
                , Outbound.none
                )
            else
                ( Packages { packagesModel | query = query, awaitingSearch = True }
                , Outbound.SearchPackages query SearchedPackagesReceived
                )

        ( Packages packagesModel, SearchedPackagesReceived packages ) ->
            if packagesModel.awaitingSearch then
                ( Packages { packagesModel | searchedPackages = packages, awaitingSearch = False }
                , Outbound.none
                )
            else
                ( model, Outbound.none )

        _ ->
            ( model, Outbound.none )


subscriptions : Model -> Inbound Msg
subscriptions model =
    Inbound.none
