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
import Ellie.Types.User as User exposing (User)
import Elm.Package exposing (Package)
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route exposing (Route(..))


type Model
    = Authenticating (Maybe Jwt) (Maybe Revision.Id)
    | Attaching Jwt (Entity User.Id User) (Maybe Revision.Id)
    | Loading Jwt (Entity User.Id User) Revision.Id (List Package)
    | Failure Exception


type alias Transition =
    Transition.Transition Model
        { token : Jwt
        , user : Entity User.Id User
        , revision : Maybe (Entity Revision.Id Revision)
        , packages : List Package
        }


init : Maybe Jwt -> Maybe Revision.Id -> ( Model, Outbound Msg )
init token revisionId =
    ( Authenticating token revisionId
    , Outbound.GetUser token UserPrepared
    )


type Msg
    = RouteChanged Route.Route
    | UserPrepared Jwt (Entity User.Id User)
    | WorkspaceAttached (List Package)
    | RevisionLoaded (Entity Revision.Id Revision)
    | ExceptionOccured Exception


update : Msg -> Model -> ( Transition, Outbound Msg )
update msg state =
    case state of
        Authenticating token revisionId ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step <| Authenticating token (Just newRevisionId)
                            , Outbound.none
                            )

                        Route.New ->
                            ( Transition.step <| Authenticating token Nothing
                            , Outbound.none
                            )

                        Route.NotFound ->
                            case revisionId of
                                Just rid ->
                                    ( Transition.step <| Authenticating token revisionId
                                    , Outbound.Redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Authenticating token revisionId
                                    , Outbound.Redirect <| Route.toString Route.New
                                    )

                UserPrepared newToken user ->
                    ( Transition.step <| Attaching newToken user revisionId
                    , Outbound.SaveToken newToken
                    )

                ExceptionOccured exception ->
                    ( Transition.step <| Failure exception
                    , Outbound.none
                    )

                _ ->
                    ( Transition.step <| Authenticating token revisionId
                    , Outbound.none
                    )

        Attaching token user revisionId ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            ( Transition.step <| Attaching token user (Just newRevisionId)
                            , Outbound.none
                            )

                        Route.New ->
                            ( Transition.step <| Attaching token user Nothing
                            , Outbound.none
                            )

                        Route.NotFound ->
                            case revisionId of
                                Just rid ->
                                    ( Transition.step <| Attaching token user (Just rid)
                                    , Outbound.Redirect <| Route.toString <| Route.Existing rid
                                    )

                                Nothing ->
                                    ( Transition.step <| Attaching token user Nothing
                                    , Outbound.Redirect <| Route.toString Route.New
                                    )

                WorkspaceAttached packages ->
                    case revisionId of
                        Just rid ->
                            ( Transition.step <| Loading token user rid packages
                            , Outbound.GetRevision rid RevisionLoaded
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
                    ( Transition.step <| Attaching token user revisionId, Outbound.none )

        Loading token user revisionId packages ->
            case msg of
                RouteChanged route ->
                    case route of
                        Route.Existing newRevisionId ->
                            if newRevisionId == revisionId then
                                ( Transition.step <| Loading token user revisionId packages
                                , Outbound.none
                                )
                            else
                                ( Transition.step <| Loading token user newRevisionId packages
                                , Outbound.GetRevision revisionId RevisionLoaded
                                )

                        Route.New ->
                            ( Transition.exit <| { token = token, user = user, revision = Nothing, packages = packages }
                            , Outbound.none
                            )

                        Route.NotFound ->
                            ( Transition.step <| Loading token user revisionId packages
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
                    ( Transition.step <| Loading token user revisionId packages
                    , Outbound.none
                    )

        _ ->
            ( Transition.step state, Outbound.none )


subscriptions : Model -> Inbound Msg
subscriptions model =
    case model of
        Attaching token _ _ ->
            Inbound.WorkspaceAttached token WorkspaceAttached

        Loading token _ _ _ ->
            Inbound.KeepWorkspaceOpen token

        _ ->
            Inbound.none
