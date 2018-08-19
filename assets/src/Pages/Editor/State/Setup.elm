module Pages.Editor.State.Setup
    exposing
        ( Model
        , Msg(..)
        , State(..)
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


type alias Model =
    { defaultRevision : Maybe Revision
    , user : User
    , state : State
    }


type State
    = AcceptingTerms { latestTerms : Int, revisionId : Maybe Revision.Id }
    | Authenticating { revisionId : Maybe Revision.Id }
    | Loading { token : Jwt, revisionId : Revision.Id }
    | Attaching { token : Jwt, revision : Maybe ( Revision.Id, Revision ) }
    | Failure String


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : User
        , revision : Revision.External
        , packages : List Package
        }


init : Route -> User -> Int -> ( Model, Command Msg )
init route user latestTerms =
    case ( route, user.acceptedTerms == Just latestTerms ) of
        ( New, True ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = Authenticating { revisionId = Nothing }
              }
            , Effects.authenticate
                |> Command.map (Result.mapError (\_ -> ()))
                |> Command.map Authenticated
            )

        ( New, False ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = AcceptingTerms { latestTerms = latestTerms, revisionId = Nothing }
              }
            , Command.none
            )

        ( Example revision, True ) ->
            ( { defaultRevision = Just revision
              , user = user
              , state = Authenticating { revisionId = Nothing }
              }
            , Command.batch
                [ Effects.authenticate
                    |> Command.map (Result.mapError (\_ -> ()))
                    |> Command.map Authenticated
                , Effects.redirect <| Route.toString Route.New
                ]
            )

        ( Example revision, False ) ->
            ( { defaultRevision = Just revision
              , user = user
              , state = AcceptingTerms { latestTerms = latestTerms, revisionId = Nothing }
              }
            , Effects.redirect <| Route.toString Route.New
            )

        ( Existing revisionId, True ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = Authenticating { revisionId = Just revisionId }
              }
            , Effects.authenticate
                |> Command.map (Result.mapError (\_ -> ()))
                |> Command.map Authenticated
            )

        ( Existing revisionId, False ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = AcceptingTerms { latestTerms = latestTerms, revisionId = Just revisionId }
              }
            , Command.none
            )

        ( NotFound, True ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = Authenticating { revisionId = Nothing }
              }
            , Command.batch
                [ Effects.authenticate
                    |> Command.map (Result.mapError (\_ -> ()))
                    |> Command.map Authenticated
                , Effects.redirect <| Route.toString Route.New
                ]
            )

        ( NotFound, False ) ->
            ( { defaultRevision = Nothing
              , user = user
              , state = AcceptingTerms { latestTerms = latestTerms, revisionId = Nothing }
              }
            , Effects.redirect <| Route.toString Route.New
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
update msg model =
    case model.state of
        AcceptingTerms state ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step
                                { model
                                    | state = AcceptingTerms { state | revisionId = Just newRevisionId }
                                }
                            , Command.none
                            )

                        Route.New ->
                            ( Transition.step
                                { model
                                    | state = AcceptingTerms { state | revisionId = Nothing }
                                }
                            , Command.none
                            )

                        Route.Example defaultRevision ->
                            ( Transition.step
                                { model
                                    | defaultRevision = Just defaultRevision
                                    , state = AcceptingTerms { state | revisionId = Nothing }
                                }
                            , Effects.redirect <| Route.toString Route.New
                            )

                        Route.NotFound ->
                            ( Transition.step
                                { model
                                    | state = AcceptingTerms state
                                }
                            , case state.revisionId of
                                Just rid ->
                                    Effects.redirect <| Route.toString <| Route.Existing rid

                                Nothing ->
                                    Effects.redirect <| Route.toString Route.New
                            )

                UserAcceptedTerms ->
                    let
                        currentUser =
                            model.user

                        updatedUser =
                            { currentUser | acceptedTerms = Just state.latestTerms }
                    in
                    ( Transition.step
                        { model
                            | user = updatedUser
                            , state = Authenticating { revisionId = state.revisionId }
                        }
                    , Command.batch
                        [ Effects.updateUser updatedUser
                        , Effects.authenticate
                            |> Command.map (Result.mapError (\_ -> ()))
                            |> Command.map Authenticated
                        ]
                    )

                _ ->
                    ( Transition.step model
                    , Command.none
                    )

        Authenticating state ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step
                                { model
                                    | state = Authenticating { revisionId = Just newRevisionId }
                                }
                            , Command.none
                            )

                        Route.New ->
                            ( Transition.step
                                { model
                                    | state = Authenticating { revisionId = Nothing }
                                }
                            , Command.none
                            )

                        Route.Example defaultRevision ->
                            ( Transition.step
                                { model
                                    | defaultRevision = Just defaultRevision
                                    , state = Authenticating { revisionId = Nothing }
                                }
                            , Effects.redirect <| Route.toString Route.New
                            )

                        Route.NotFound ->
                            ( Transition.step
                                { model
                                    | state = Authenticating { revisionId = state.revisionId }
                                }
                            , case state.revisionId of
                                Just rid ->
                                    Effects.redirect <| Route.toString <| Route.Existing rid

                                Nothing ->
                                    Effects.redirect <| Route.toString Route.New
                            )

                Authenticated (Ok token) ->
                    case state.revisionId of
                        Just revisionId ->
                            ( Transition.step
                                { model
                                    | state = Loading { token = token, revisionId = revisionId }
                                }
                            , Effects.getRevision revisionId
                                |> Command.map (Result.mapError (\_ -> ()))
                                |> Command.map (RevisionLoaded revisionId)
                            )

                        Nothing ->
                            ( Transition.step
                                { model
                                    | state = Attaching { token = token, revision = Nothing }
                                }
                            , Command.none
                            )

                _ ->
                    ( Transition.step model
                    , Command.none
                    )

        Loading state ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if newRevisionId == state.revisionId then
                                ( Transition.step model
                                , Command.none
                                )
                            else
                                ( Transition.step
                                    { model
                                        | state = Loading { state | revisionId = newRevisionId }
                                    }
                                , Effects.getRevision newRevisionId
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded newRevisionId)
                                )

                        Route.New ->
                            ( Transition.step
                                { model
                                    | state = Attaching { token = state.token, revision = Nothing }
                                }
                            , Command.none
                            )

                        Route.Example defaultRevision ->
                            ( Transition.step
                                { model
                                    | defaultRevision = Just defaultRevision
                                    , state = Attaching { token = state.token, revision = Nothing }
                                }
                            , Effects.redirect <| Route.toString Route.New
                            )

                        Route.NotFound ->
                            ( Transition.step model
                            , Effects.redirect <| Route.toString <| Route.Existing state.revisionId
                            )

                RevisionLoaded rid result ->
                    if rid == state.revisionId then
                        case result of
                            Ok revision ->
                                ( Transition.step
                                    { model
                                        | state = Attaching { token = state.token, revision = Just ( rid, revision ) }
                                    }
                                , Command.none
                                )

                            Err _ ->
                                ( Transition.step
                                    { model
                                        | state = Failure "Could not load revision"
                                    }
                                , Command.none
                                )
                    else
                        ( Transition.step model
                        , Command.none
                        )

                _ ->
                    ( Transition.step model
                    , Command.none
                    )

        Attaching state ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if Maybe.map Tuple.first state.revision == Just newRevisionId then
                                ( Transition.step model
                                , Command.none
                                )
                            else
                                ( Transition.step
                                    { model
                                        | state = Loading { token = state.token, revisionId = newRevisionId }
                                    }
                                , Effects.getRevision newRevisionId
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded newRevisionId)
                                )

                        Route.New ->
                            ( Transition.step
                                { model
                                    | state = Attaching { state | revision = Nothing }
                                }
                            , Command.none
                            )

                        Route.Example defaultRevision ->
                            ( Transition.step
                                { model
                                    | state = Attaching { state | revision = Nothing }
                                    , defaultRevision = Just defaultRevision
                                }
                            , Effects.redirect <| Route.toString Route.New
                            )

                        Route.NotFound ->
                            case state.revision of
                                Just ( rid, _ ) ->
                                    ( Transition.step model
                                    , Effects.redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step model
                                    , Effects.redirect <| Route.toString Route.New
                                    )

                WorkspaceConnected ->
                    ( Transition.step model
                    , state.revision
                        |> Maybe.map (Tuple.second >> .elmVersion)
                        |> Maybe.withDefault Compiler.version
                        |> Effects.attachToWorkspace state.token
                        |> Command.map (\_ -> NoOp)
                    )

                WorkspaceAttached packages ->
                    ( Transition.exit
                        { token = state.token
                        , user = model.user
                        , packages = packages
                        , revision =
                            case ( model.defaultRevision, state.revision ) of
                                ( _, Just revisionAndId ) ->
                                    Revision.Remote revisionAndId

                                ( Just default, Nothing ) ->
                                    Revision.Example default

                                ( Nothing, Nothing ) ->
                                    Revision.Default
                        }
                    , Command.none
                    )

                _ ->
                    ( Transition.step model
                    , Command.none
                    )

        _ ->
            ( Transition.step model, Command.none )


subscriptions : Model -> Subscription Msg
subscriptions model =
    case model.state of
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
