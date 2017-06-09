module Apps.Embed.Update
    exposing
        ( update
        , initialize
        , onRouteChange
        , Msg(..)
        )

import Navigation
import RemoteData exposing (RemoteData(..))
import Apps.Embed.Model as Model exposing (Model, Tab(..))
import Apps.Embed.Routing as Routing exposing (Route(..))
import Shared.Api as Api
import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)


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

        LoadRevisionCompleted result ->
            ( { model | revision = RemoteData.fromResult result }
            , result
                |> Result.map (\_ -> False)
                |> resultOnError (\apiError -> Ok <| apiError.statusCode == 404)
                |> Result.map (choose (Navigation.modifyUrl <| Routing.construct NotFound) Cmd.none)
                |> Result.withDefault Cmd.none
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
    in
        location
            |> Routing.parse
            |> (\route -> { initialModel | currentRoute = route })
            |> (\model -> ( model, Cmd.none ))
            |> (\( model, cmd ) -> handleRouteChanged model.currentRoute ( model, cmd ))



------------------------


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
