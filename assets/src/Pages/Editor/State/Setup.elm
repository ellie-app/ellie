module Pages.Editor.State.Setup
    exposing
        ( Model(..)
        , Msg(..)
        , Transition
        , init
        , subscriptions
        , update
        )

import Data.Entity as Entity exposing (Entity(..))
import Data.Jwt as Jwt exposing (Jwt)
import Data.Transition as Transition
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Types.User as User exposing (User)
import Elm.Package exposing (Package)
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route exposing (Route(..))


type Model
    = Authenticating { possibleToken : Maybe Jwt, revisionId : Maybe Revision.Id }
    | AcceptingTerms { latestTerms : TermsVersion, token : Jwt, user : Entity User.Id User, revisionId : Maybe Revision.Id, loading : Bool }
    | Attaching { token : Jwt, user : Entity User.Id User, revisionId : Maybe Revision.Id }
    | Loading { token : Jwt, user : Entity User.Id User, revisionId : Revision.Id, packages : List Package }
    | Failure Exception


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : Entity User.Id User
        , revision : Maybe (Entity Revision.Id Revision)
        , packages : List Package
        }


init : Maybe Jwt -> Maybe Revision.Id -> ( Model, Outbound Msg )
init possibleToken revisionId =
    ( Authenticating { possibleToken = possibleToken, revisionId = revisionId }
    , Outbound.Authenticate possibleToken UserPrepared
    )


type Msg
    = RouteChanged Route.Route
    | UserPrepared TermsVersion Jwt (Entity User.Id User)
    | UserAcceptedTerms
    | ServerAcceptedTerms
    | WorkspaceAttached (List Package)
    | RevisionLoaded (Entity Revision.Id Revision)
    | ExceptionOccured Exception


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
                            user
                                |> Entity.record
                                |> .termsVersion
                                |> Maybe.map (TermsVersion.eq termsVersion)
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

        AcceptingTerms state ->
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
                      , user = Entity.map (\u -> { u | termsVersion = Just state.latestTerms }) state.user
                      }
                        |> Attaching
                        |> Transition.step
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| AcceptingTerms state
                    , Outbound.none
                    )

        Attaching { token, user, revisionId } ->
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

                RevisionLoaded ((Entity rid revision) as entity) ->
                    ( Transition.exit { token = token, user = user, revision = Just entity, packages = packages }
                    , Outbound.GetRevision rid RevisionLoaded
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
            Inbound.WorkspaceAttached token WorkspaceAttached

        Loading { token } ->
            Inbound.KeepWorkspaceOpen token

        _ ->
            Inbound.none
