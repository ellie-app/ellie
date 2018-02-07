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
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route exposing (Route(..))


type Model
    = Authenticating (Maybe Jwt) (Maybe Revision.Id)
    | Attaching Jwt (Maybe Revision.Id)
    | Loading Jwt Revision.Id (List Package)
    | Failure String


type alias Transition =
    Transition.Transition Model
        { token : Jwt
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


update : Msg -> Model -> ( Transition, Outbound Msg )
update msg state =
    case state of
        Authenticating token revisionId ->
            updateAuthenticating msg token revisionId

        Attaching token revisionId ->
            updateAttaching msg token revisionId

        Loading token revisionId packages ->
            updateLoading msg token revisionId packages

        _ ->
            ( Transition.step state, Outbound.none )


updateAuthenticating : Msg -> Maybe Jwt -> Maybe Revision.Id -> ( Transition, Outbound Msg )
updateAuthenticating msg token revisionId =
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
            ( Transition.step <| Attaching newToken revisionId
            , Outbound.SaveToken newToken
            )

        _ ->
            ( Transition.step <| Authenticating token revisionId
            , Outbound.none
            )


updateAttaching : Msg -> Jwt -> Maybe Revision.Id -> ( Transition, Outbound Msg )
updateAttaching msg token revisionId =
    case msg of
        RouteChanged route ->
            case route of
                Route.Existing newRevisionId ->
                    ( Transition.step <| Attaching token (Just newRevisionId)
                    , Outbound.none
                    )

                Route.New ->
                    ( Transition.step <| Attaching token Nothing
                    , Outbound.none
                    )

                Route.NotFound ->
                    case revisionId of
                        Just rid ->
                            ( Transition.step <| Attaching token (Just rid)
                            , Outbound.Redirect <| Route.toString <| Route.Existing rid
                            )

                        Nothing ->
                            ( Transition.step <| Attaching token Nothing
                            , Outbound.Redirect <| Route.toString Route.New
                            )

        WorkspaceAttached packages ->
            case revisionId of
                Just rid ->
                    ( Transition.step <| Loading token rid packages
                    , Outbound.GetRevision rid RevisionLoaded
                    )

                Nothing ->
                    ( Transition.exit { token = token, revision = Nothing, packages = packages }
                    , Outbound.none
                    )

        _ ->
            ( Transition.step <| Attaching token revisionId, Outbound.none )


updateLoading : Msg -> Jwt -> Revision.Id -> List Package -> ( Transition, Outbound Msg )
updateLoading msg token revisionId packages =
    case msg of
        RouteChanged route ->
            case route of
                Route.Existing newRevisionId ->
                    if newRevisionId == revisionId then
                        ( Transition.step <| Loading token revisionId packages
                        , Outbound.none
                        )
                    else
                        ( Transition.step <| Loading token newRevisionId packages
                        , Outbound.GetRevision revisionId RevisionLoaded
                        )

                Route.New ->
                    ( Transition.exit <| { token = token, revision = Nothing, packages = packages }
                    , Outbound.none
                    )

                Route.NotFound ->
                    ( Transition.step <| Loading token revisionId packages
                    , Outbound.Redirect <| Route.toString <| Route.Existing revisionId
                    )

        RevisionLoaded ((Entity rid revision) as entity) ->
            ( Transition.exit { token = token, revision = Just entity, packages = packages }
            , Outbound.GetRevision rid RevisionLoaded
            )

        _ ->
            ( Transition.step <| Loading token revisionId packages
            , Outbound.none
            )


subscriptions : Model -> Inbound Msg
subscriptions model =
    case model of
        Attaching token _ ->
            Inbound.WorkspaceAttached token WorkspaceAttached

        Loading token _ _ ->
            Inbound.KeepWorkspaceOpen token

        _ ->
            Inbound.none
