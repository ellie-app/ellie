module Pages.Editor.State.Actions exposing (Model(..), Msg(..), PackagesModel, packages, subscriptions, update)

import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Elm.Package as Package exposing (Package)
import Pages.Editor.Effects as Effects


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


update : Msg -> Model -> ( Model, Command Msg )
update msg model =
    case ( model, msg ) of
        ( Packages packagesModel, UserTypedInPackageSearch query ) ->
            if String.isEmpty query then
                ( Packages { packagesModel | query = query, searchedPackages = Nothing, awaitingSearch = False }
                , Command.none
                )

            else if String.length query < 4 then
                ( Packages { packagesModel | query = query }
                , Command.none
                )

            else
                ( Packages { packagesModel | query = query, awaitingSearch = True }
                , Effects.searchPackages query
                    |> Command.map Result.toMaybe
                    |> Command.map SearchedPackagesReceived
                )

        ( Packages packagesModel, SearchedPackagesReceived searchedPackages ) ->
            if packagesModel.awaitingSearch then
                ( Packages { packagesModel | searchedPackages = searchedPackages, awaitingSearch = False }
                , Command.none
                )

            else
                ( model, Command.none )

        _ ->
            ( model, Command.none )


subscriptions : Model -> Subscription Msg
subscriptions model =
    Subscription.none
