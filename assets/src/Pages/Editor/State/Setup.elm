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
    | Attaching { token : Jwt, user : User, revisionId : Maybe RevisionId }
    | Loading { token : Jwt, user : User, revisionId : RevisionId, packages : List Package }
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
    | RevisionLoaded Revision
    | ExceptionOccured Exception
    | NoOp


update : Msg -> Model -> ( Transition, Outbound Msg )
update msg state =
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
                    if termsVersionMatched then
                        ( Transition.step <| Attaching { token = newToken, user = user, revisionId = revisionId }
                        , Outbound.SaveToken newToken
                        )
                    else
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
                    ( { token = state.token
                      , revisionId = state.revisionId
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

        Attaching ({ token, user, revisionId } as attachingState) ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step <| Attaching { token = token, user = user, revisionId = Just newRevisionId }
                            , Outbound.none
                            )

                        Route.New ->
                            ( Transition.step <| Attaching { token = token, user = user, revisionId = Nothing }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            case revisionId of
                                Just rid ->
                                    ( Transition.step <| Attaching { token = token, user = user, revisionId = Just rid }
                                    , Outbound.Redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Attaching { token = token, user = user, revisionId = Nothing }
                                    , Outbound.Redirect <| Route.toString Route.New
                                    )

                WorkspaceConnected ->
                    ( Transition.step <| Attaching attachingState
                    , Outbound.AttachToWorkspace token Compiler.version
                    )

                WorkspaceAttached packages ->
                    case revisionId of
                        Just revisionId ->
                            ( Transition.step <| Loading { token = token, user = user, revisionId = revisionId, packages = packages }
                            , Outbound.GetRevision revisionId RevisionLoaded
                            )

                        Nothing ->
                            ( Transition.exit { token = token, user = user, revision = Nothing, packages = packages }
                            , Outbound.none
                            )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Attaching { token = token, user = user, revisionId = revisionId }
                    , Outbound.none
                    )

        Loading { token, user, revisionId, packages } ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if newRevisionId == revisionId then
                                ( Transition.step <| Loading { token = token, user = user, revisionId = revisionId, packages = packages }
                                , Outbound.none
                                )
                            else
                                ( Transition.step <| Loading { token = token, user = user, revisionId = newRevisionId, packages = packages }
                                , Outbound.GetRevision revisionId RevisionLoaded
                                )

                        Route.New ->
                            ( Transition.exit <| { token = token, user = user, revision = Nothing, packages = packages }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            ( Transition.step <| Loading { token = token, user = user, revisionId = revisionId, packages = packages }
                            , Outbound.Redirect <| Route.toString <| Route.Existing revisionId
                            )

                RevisionLoaded revision ->
                    ( Transition.exit { token = token, user = user, revision = Just ( revisionId, revision ), packages = packages }
                    , Outbound.none
                    )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Loading { token = token, user = user, revisionId = revisionId, packages = packages }
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
