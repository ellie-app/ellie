module Components.Sidebar.Update exposing (Msg(..), initialize, update)

import LruCache exposing (LruCache)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Components.Sidebar.Model as Model exposing (Model, SearchFlow(..))
import Components.PackageSearch.Update as PackageSearch


type alias PackagesCache =
    LruCache String (List PackageSearchResult)


type Msg
    = NoOp
    | DetailsToggled
    | PackagesToggled
    | NewSearchStarted
    | SearchCanceled
    | PackageSearchMsg PackageSearch.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DetailsToggled ->
            ( model
                |> Model.toggleDetails
            , Cmd.none
            )

        PackagesToggled ->
            ( model
                |> Model.togglePackages
            , Cmd.none
            )

        NewSearchStarted ->
            ( model
                |> Model.startNewPackageSearch
            , Cmd.none
            )

        SearchCanceled ->
            ( model
                |> Model.cancelSearch
            , Cmd.none
            )

        PackageSearchMsg childMsg ->
            case model.searchFlow of
                NewPackageSearching searchModel ->
                    let
                        ( childModel, childCmd ) =
                            PackageSearch.update childMsg searchModel
                    in
                        ( { model | searchFlow = NewPackageSearching childModel }
                        , Cmd.map PackageSearchMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )


initialize : Cmd Msg
initialize =
    Cmd.none
