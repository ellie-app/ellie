module Pages.Editor.State.Setup
    exposing
        ( Model(..)
        , Msg(..)
        , Transition
        , init
        , subscriptions
        , update
        )

import Data.Jwt as Jwt exposing (Jwt)
import Data.Transition as Transition
import Elm.Compiler as Compiler
import Elm.Package exposing (Package)
import Extra.Maybe as Maybe
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route exposing (Route(..))
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)


type Model
    = Authenticating { possibleToken : Maybe Jwt, revisionId : Maybe RevisionId }
    | AcceptingTerms { latestTerms : Int, token : Jwt, user : User, revisionId : Maybe RevisionId, loading : Bool }
    | Loading { token : Jwt, user : User, revisionId : RevisionId }
    | Attaching { token : Jwt, user : User, revision : Maybe ( RevisionId, Revision ) }
    | Failure Exception


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : User
        , revision : Maybe ( RevisionId, Revision )
        , packages : List Package
        }


init : Maybe Jwt -> Maybe RevisionId -> ( Model, Outbound Msg )
init possibleToken revisionId =
    ( Authenticating { possibleToken = possibleToken, revisionId = revisionId }
    , Outbound.Authenticate possibleToken UserPrepared
    )


type Msg
    = RouteChanged Route.Route
    | UserPrepared Int Jwt User
    | UserAcceptedTerms
    | ServerAcceptedTerms
    | WorkspaceConnected
    | WorkspaceAttached (List Package)
    | RevisionLoaded RevisionId Revision
    | ExceptionOccured Exception
    | NoOp


update : Msg -> Model -> ( Transition, Outbound Msg )
update msg state =
    let
        _ =
            Debug.log (toString msg) state
    in
    case state of
        Authenticating { possibleToken, revisionId } ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = Just newRevisionId }
                            , Outbound.none
                            )

                        Route.New ->
                            ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = Nothing }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            case revisionId of
                                Just rid ->
                                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                                    , Outbound.Redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                                    , Outbound.Redirect <| Route.toString Route.New
                                    )

                UserPrepared termsVersion newToken user ->
                    let
                        termsVersionMatched =
                            user.acceptedTerms
                                |> Maybe.map ((==) termsVersion)
                                |> Maybe.withDefault False
                    in
                    case ( termsVersionMatched, revisionId ) of
                        ( True, Just rid ) ->
                            ( Transition.step <| Loading { token = newToken, user = user, revisionId = rid }
                            , Outbound.batch
                                [ Outbound.SaveToken newToken
                                , Outbound.GetRevision rid (RevisionLoaded rid)
                                ]
                            )

                        ( True, Nothing ) ->
                            ( Transition.step <| Attaching { token = newToken, user = user, revision = Nothing }
                            , Outbound.SaveToken newToken
                            )

                        _ ->
                            ( { latestTerms = termsVersion
                              , token = newToken
                              , user = user
                              , revisionId = revisionId
                              , loading = False
                              }
                                |> AcceptingTerms
                                |> Transition.step
                            , Outbound.SaveToken newToken
                            )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                    , Outbound.none
                    )

        AcceptingTerms ({ user } as state) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( { state | revisionId = Just newRevisionId }
                                |> AcceptingTerms
                                |> Transition.step
                            , Outbound.none
                            )

                        Route.New ->
                            ( { state | revisionId = Nothing }
                                |> AcceptingTerms
                                |> Transition.step
                            , Outbound.none
                            )

                        Route.NotFound ->
                            ( { state | revisionId = Nothing }
                                |> AcceptingTerms
                                |> Transition.step
                            , case state.revisionId of
                                Just rid ->
                                    Outbound.Redirect <| Route.toString <| Route.Existing rid

                                Nothing ->
                                    Outbound.Redirect <| Route.toString Route.New
                            )

                UserAcceptedTerms ->
                    ( { state | loading = True }
                        |> AcceptingTerms
                        |> Transition.step
                    , Outbound.AcceptTerms state.token state.latestTerms ServerAcceptedTerms
                    )

                ServerAcceptedTerms ->
                    case state.revisionId of
                        Just revisionId ->
                            ( { token = state.token
                              , user = { user | acceptedTerms = Just state.latestTerms }
                              , revisionId = revisionId
                              }
                                |> Loading
                                |> Transition.step
                            , Outbound.GetRevision revisionId (RevisionLoaded revisionId)
                            )

                        Nothing ->
                            ( { token = state.token
                              , revision = Nothing
                              , user = { user | acceptedTerms = Just state.latestTerms }
                              }
                                |> Attaching
                                |> Transition.step
                            , Outbound.none
                            )

                _ ->
                    ( Transition.step <| AcceptingTerms state
                    , Outbound.none
                    )

        Loading ({ token, user, revisionId } as state) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if newRevisionId == revisionId then
                                ( Transition.step <| Loading state
                                , Outbound.none
                                )
                            else
                                ( Transition.step <| Loading { state | revisionId = newRevisionId }
                                , Outbound.GetRevision revisionId (RevisionLoaded revisionId)
                                )

                        Route.New ->
                            ( Transition.step <| Attaching { token = token, user = user, revision = Nothing }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            ( Transition.step <| Loading state
                            , Outbound.Redirect <| Route.toString <| Route.Existing revisionId
                            )

                RevisionLoaded rid revision ->
                    if RevisionId.eq rid state.revisionId then
                        ( Transition.step <| Attaching { token = state.token, user = state.user, revision = Just ( rid, revision ) }
                        , Outbound.none
                        )
                    else
                        ( Transition.step <| Loading state
                        , Outbound.none
                        )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Loading state
                    , Outbound.none
                    )

        Attaching ({ token, user, revision } as attachingState) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if Maybe.eq RevisionId.eq (Maybe.map Tuple.first revision) (Just newRevisionId) then
                                ( Transition.step <| Attaching attachingState
                                , Outbound.none
                                )
                            else
                                ( Transition.step <| Loading { token = token, user = user, revisionId = newRevisionId }
                                , Outbound.GetRevision newRevisionId (RevisionLoaded newRevisionId)
                                )

                        Route.New ->
                            ( Transition.step <| Attaching { attachingState | revision = Nothing }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            case revision of
                                Just ( rid, _ ) ->
                                    ( Transition.step <| Attaching attachingState
                                    , Outbound.Redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Attaching attachingState
                                    , Outbound.Redirect <| Route.toString Route.New
                                    )

                WorkspaceConnected ->
                    ( Transition.step <| Attaching attachingState
                    , revision
                        |> Maybe.map (Tuple.second >> .elmVersion)
                        |> Maybe.withDefault Compiler.version
                        |> Debug.log "ob"
                        |> Outbound.AttachToWorkspace token
                    )

                WorkspaceAttached packages ->
                    ( Transition.exit { token = token, user = user, revision = revision, packages = packages }
                    , Outbound.none
                    )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Attaching attachingState
                    , Outbound.none
                    )

        _ ->
            ( Transition.step state, Outbound.none )


subscriptions : Model -> Inbound Msg
subscriptions model =
    case model of
        Attaching { token } ->
            Inbound.WorkspaceUpdates token chooseUpdate

        Loading { token } ->
            Inbound.WorkspaceUpdates token chooseUpdate

        _ ->
            Inbound.none


chooseUpdate : WorkspaceUpdate -> Msg
chooseUpdate update =
    case update of
        WorkspaceUpdate.Attached packages ->
            WorkspaceAttached packages

        WorkspaceUpdate.Connected ->
            WorkspaceConnected

        _ ->
            NoOp
