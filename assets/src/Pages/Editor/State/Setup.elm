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
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Elm.Compiler as Compiler
import Elm.Package exposing (Package)
import Extra.Maybe as Maybe
import Pages.Editor.Effects as Effects
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
    | Failure String


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : User
        , revision : Maybe ( RevisionId, Revision )
        , packages : List Package
        }


init : Maybe Jwt -> Maybe RevisionId -> ( Model, Command Msg )
init possibleToken revisionId =
    ( Authenticating { possibleToken = possibleToken, revisionId = revisionId }
    , Effects.authenticate possibleToken
        |> Command.map (Result.mapError (\_ -> ()))
        |> Command.map UserPrepared
    )


type Msg
    = RouteChanged Route.Route
    | UserPrepared (Result () ( Int, Jwt, User ))
    | UserAcceptedTerms
    | ServerAcceptedTerms (Result () ())
    | WorkspaceConnected
    | WorkspaceAttached (List Package)
    | RevisionLoaded RevisionId (Result () Revision)
    | NoOp


update : Msg -> Model -> ( Transition, Command Msg )
update msg state =
    case state of
        Authenticating { possibleToken, revisionId } ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = Just newRevisionId }
                            , Command.none
                            )

                        Route.New ->
                            ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = Nothing }
                            , Command.none
                            )

                        Route.NotFound ->
                            case revisionId of
                                Just rid ->
                                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                                    , Effects.redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                                    , Effects.redirect <| Route.toString Route.New
                                    )

                UserPrepared (Ok ( termsVersion, newToken, user )) ->
                    let
                        termsVersionMatched =
                            user.acceptedTerms
                                |> Maybe.map ((==) termsVersion)
                                |> Maybe.withDefault False
                    in
                    case ( termsVersionMatched, revisionId ) of
                        ( True, Just rid ) ->
                            ( Transition.step <| Loading { token = newToken, user = user, revisionId = rid }
                            , Command.batch
                                [ Effects.saveToken newToken
                                , Effects.getRevision rid
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded rid)
                                ]
                            )

                        ( True, Nothing ) ->
                            ( Transition.step <| Attaching { token = newToken, user = user, revision = Nothing }
                            , Effects.saveToken newToken
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
                            , Effects.saveToken newToken
                            )

                UserPrepared (Err _) ->
                    ( Transition.step <| Failure "Could not authenticate user"
                    , Command.none
                    )

                _ ->
                    ( Transition.step <| Authenticating { possibleToken = possibleToken, revisionId = revisionId }
                    , Command.none
                    )

        AcceptingTerms ({ user } as state) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( { state | revisionId = Just newRevisionId }
                                |> AcceptingTerms
                                |> Transition.step
                            , Command.none
                            )

                        Route.New ->
                            ( { state | revisionId = Nothing }
                                |> AcceptingTerms
                                |> Transition.step
                            , Command.none
                            )

                        Route.NotFound ->
                            ( { state | revisionId = Nothing }
                                |> AcceptingTerms
                                |> Transition.step
                            , case state.revisionId of
                                Just rid ->
                                    Effects.redirect <| Route.toString <| Route.Existing rid

                                Nothing ->
                                    Effects.redirect <| Route.toString Route.New
                            )

                UserAcceptedTerms ->
                    ( { state | loading = True }
                        |> AcceptingTerms
                        |> Transition.step
                    , Effects.acceptTerms state.token state.latestTerms
                        |> Command.map (Result.mapError (\_ -> ()))
                        |> Command.map ServerAcceptedTerms
                    )

                ServerAcceptedTerms (Ok _) ->
                    case state.revisionId of
                        Just revisionId ->
                            ( { token = state.token
                              , user = { user | acceptedTerms = Just state.latestTerms }
                              , revisionId = revisionId
                              }
                                |> Loading
                                |> Transition.step
                            , Effects.getRevision revisionId
                                |> Command.map (Result.mapError (\_ -> ()))
                                |> Command.map (RevisionLoaded revisionId)
                            )

                        Nothing ->
                            ( { token = state.token
                              , revision = Nothing
                              , user = { user | acceptedTerms = Just state.latestTerms }
                              }
                                |> Attaching
                                |> Transition.step
                            , Command.none
                            )

                ServerAcceptedTerms (Err _) ->
                    ( Transition.step <| Failure "Could not accept terms"
                    , Command.none
                    )

                _ ->
                    ( Transition.step <| AcceptingTerms state
                    , Command.none
                    )

        Loading ({ token, user, revisionId } as state) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if newRevisionId == revisionId then
                                ( Transition.step <| Loading state
                                , Command.none
                                )
                            else
                                ( Transition.step <| Loading { state | revisionId = newRevisionId }
                                , Effects.getRevision revisionId
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded newRevisionId)
                                )

                        Route.New ->
                            ( Transition.step <| Attaching { token = token, user = user, revision = Nothing }
                            , Command.none
                            )

                        Route.NotFound ->
                            ( Transition.step <| Loading state
                            , Effects.redirect <| Route.toString <| Route.Existing revisionId
                            )

                RevisionLoaded rid result ->
                    if RevisionId.eq rid state.revisionId then
                        case result of
                            Ok revision ->
                                ( Transition.step <| Attaching { token = state.token, user = state.user, revision = Just ( rid, revision ) }
                                , Command.none
                                )

                            Err _ ->
                                ( Transition.step <| Failure "Could not load revision"
                                , Command.none
                                )
                    else
                        ( Transition.step <| Loading state
                        , Command.none
                        )

                _ ->
                    ( Transition.step <| Loading state
                    , Command.none
                    )

        Attaching ({ token, user, revision } as attachingState) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if Maybe.eq RevisionId.eq (Maybe.map Tuple.first revision) (Just newRevisionId) then
                                ( Transition.step <| Attaching attachingState
                                , Command.none
                                )
                            else
                                ( Transition.step <| Loading { token = token, user = user, revisionId = newRevisionId }
                                , Effects.getRevision newRevisionId
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded newRevisionId)
                                )

                        Route.New ->
                            ( Transition.step <| Attaching { attachingState | revision = Nothing }
                            , Command.none
                            )

                        Route.NotFound ->
                            case revision of
                                Just ( rid, _ ) ->
                                    ( Transition.step <| Attaching attachingState
                                    , Effects.redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Attaching attachingState
                                    , Effects.redirect <| Route.toString Route.New
                                    )

                WorkspaceConnected ->
                    ( Transition.step <| Attaching attachingState
                    , revision
                        |> Maybe.map (Tuple.second >> .elmVersion)
                        |> Maybe.withDefault Compiler.version
                        |> Effects.attachToWorkspace token
                        |> Command.map (\_ -> NoOp)
                    )

                WorkspaceAttached packages ->
                    ( Transition.exit { token = token, user = user, revision = revision, packages = packages }
                    , Command.none
                    )

                _ ->
                    ( Transition.step <| Attaching attachingState
                    , Command.none
                    )

        _ ->
            ( Transition.step state, Command.none )


subscriptions : Model -> Subscription Msg
subscriptions model =
    case model of
        Attaching { token } ->
            Effects.workspaceUpdates token
                |> Subscription.map chooseUpdate

        _ ->
            Subscription.none


chooseUpdate : WorkspaceUpdate -> Msg
chooseUpdate update =
    case update of
        WorkspaceUpdate.Attached packages ->
            WorkspaceAttached packages

        WorkspaceUpdate.Connected ->
            WorkspaceConnected

        _ ->
            NoOp
