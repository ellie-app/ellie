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
import Pages.Editor.Effects as Effects
import Pages.Editor.Route as Route exposing (Route(..))
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)


type Model
    = AcceptingTerms { latestTerms : Int, user : User, revisionId : Maybe Revision.Id }
    | Authenticating { user : User, revisionId : Maybe Revision.Id }
    | Loading { token : Jwt, user : User, revisionId : Revision.Id }
    | Attaching { token : Jwt, user : User, revision : Maybe ( Revision.Id, Revision ) }
    | Failure String


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : User
        , revision : Maybe ( Revision.Id, Revision )
        , packages : List Package
        }


init : User -> Maybe Revision.Id -> Int -> ( Model, Command Msg )
init user maybeRevisionId latestTerms =
    if user.acceptedTerms == Just latestTerms then
        ( Authenticating { user = user, revisionId = maybeRevisionId }
        , Effects.authenticate
            |> Command.map (Result.mapError (\_ -> ()))
            |> Command.map Authenticated
        )
    else
        ( AcceptingTerms { latestTerms = latestTerms, user = user, revisionId = maybeRevisionId }
        , Command.none
        )


type Msg
    = RouteChanged Route.Route
    | Authenticated (Result () Jwt)
    | UserAcceptedTerms
    | WorkspaceConnected
    | WorkspaceAttached (List Package)
    | RevisionLoaded Revision.Id (Result () Revision)
    | NoOp


update : Msg -> Model -> ( Transition, Command Msg )
update msg state =
    case state of
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
                    let
                        updatedUser =
                            { user | acceptedTerms = Just state.latestTerms }
                    in
                    ( { user = updatedUser, revisionId = state.revisionId }
                        |> Authenticating
                        |> Transition.step
                    , Command.batch
                        [ Effects.updateUser updatedUser
                        , Effects.authenticate
                            |> Command.map (Result.mapError (\_ -> ()))
                            |> Command.map Authenticated
                        ]
                    )

                _ ->
                    ( Transition.step <| AcceptingTerms state
                    , Command.none
                    )

        Authenticating ({ user, revisionId } as state) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( { state | revisionId = Just newRevisionId }
                                |> Authenticating
                                |> Transition.step
                            , Command.none
                            )

                        Route.New ->
                            ( { state | revisionId = Nothing }
                                |> Authenticating
                                |> Transition.step
                            , Command.none
                            )

                        Route.NotFound ->
                            ( { state | revisionId = Nothing }
                                |> Authenticating
                                |> Transition.step
                            , case state.revisionId of
                                Just rid ->
                                    Effects.redirect <| Route.toString <| Route.Existing rid

                                Nothing ->
                                    Effects.redirect <| Route.toString Route.New
                            )

                Authenticated (Ok token) ->
                    case state.revisionId of
                        Just revisionId ->
                            ( { token = token, user = user, revisionId = revisionId }
                                |> Loading
                                |> Transition.step
                            , Effects.getRevision revisionId
                                |> Command.map (Result.mapError (\_ -> ()))
                                |> Command.map (RevisionLoaded revisionId)
                            )

                        Nothing ->
                            ( { token = token, revision = Nothing, user = user }
                                |> Attaching
                                |> Transition.step
                            , Command.none
                            )

                _ ->
                    ( Transition.step <| Authenticating state
                    , Command.none
                    )

        Loading ({ user, revisionId, token } as state) ->
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
                            ( Transition.step <| Attaching { user = user, token = token, revision = Nothing }
                            , Command.none
                            )

                        Route.NotFound ->
                            ( Transition.step <| Loading state
                            , Effects.redirect <| Route.toString <| Route.Existing revisionId
                            )

                RevisionLoaded rid result ->
                    if rid == state.revisionId then
                        case result of
                            Ok revision ->
                                ( Transition.step <| Attaching { user = user, token = token, revision = Just ( rid, revision ) }
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

        Attaching ({ user, revision, token } as attachingState) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if Maybe.map Tuple.first revision == Just newRevisionId then
                                ( Transition.step <| Attaching attachingState
                                , Command.none
                                )
                            else
                                ( Transition.step <| Loading { user = user, token = token, revisionId = newRevisionId }
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
                        |> Effects.attachToWorkspace attachingState.token
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
