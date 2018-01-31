module Pages.Embed.Update
    exposing
        ( Msg(..)
        , initialize
        , onRouteChange
        , update
        )

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Ellie.Api as Api
import Navigation
import Pages.Embed.Model as Model exposing (Model, Tab(..))
import Pages.Embed.Routing as Routing exposing (Route(..))
import RemoteData exposing (RemoteData(..))


resultOnError : (x -> Result y a) -> Result x a -> Result y a
resultOnError callback result =
    case result of
        Ok a ->
            Ok a

        Err x ->
            callback x


choose : a -> a -> Bool -> a
choose l r predicate =
    if predicate then
        l
    else
        r


type Msg
    = NoOp
    | RouteChanged Route
    | LoadRevisionCompleted (Result ApiError Revision)
    | SwitchTab Tab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchTab tab ->
            ( { model | tab = tab }, Cmd.none )

        LoadRevisionCompleted (Ok revision) ->
            ( { model | revision = RemoteData.Success revision }
            , Cmd.none
            )

        LoadRevisionCompleted (Err apiError) ->
            ( { model | revision = RemoteData.Failure apiError }
            , if apiError.statusCode == 404 then
                Navigation.modifyUrl <| Routing.construct NotFound
              else
                Cmd.none
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        _ ->
            ( model, Cmd.none )


onRouteChange : Navigation.Location -> Msg
onRouteChange =
    Routing.parse >> RouteChanged


initialize : Navigation.Location -> ( Model, Cmd Msg )
initialize location =
    let
        initialModel =
            Model.model

        model =
            { initialModel | currentRoute = Routing.parse location }
    in
    handleRouteChanged
        model.currentRoute
        ( model
        , Cmd.none
        )


loadRevision : RevisionId -> Cmd Msg
loadRevision revisionId =
    Api.exactRevision revisionId
        |> Api.send LoadRevisionCompleted


handleRouteChanged : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRouteChanged route ( model, cmd ) =
    case route of
        SpecificRevision revisionId ->
            ( { model | revision = Loading }
            , Cmd.batch
                [ cmd
                , loadRevision revisionId
                ]
            )

        NotFound ->
            ( model
            , cmd
            )
