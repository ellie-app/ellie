module Pages.Editor.Update
    exposing
        ( Msg(..)
        , subscriptions
        , update
        )

import Data.Entity exposing (Entity(..))
import Data.Jwt exposing (Jwt)
import Ellie.Effect.Inbound as Inbound exposing (Inbound)
import Ellie.Effect.Outbound as Outbound exposing (Outbound)
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.User as User exposing (User)
import Elm.Package as Package exposing (Package)
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Route as Route exposing (Route)


type Msg
    = NoOp
    | AppStart
    | RouteChanged Route
    | UserPrepared Jwt (Entity User.Id User)
    | RevisionReceived (Entity Revision.Id Revision)
    | WorkspaceAttached (List Package)
    | ElmCodeChanged String
    | HtmlCodeChanged String
    | ErrorOccured String


update : Msg -> Model -> ( Model, List (Outbound Msg) )
update msg model =
    case msg of
        RouteChanged route ->
            ( { model | route = route }
            , []
            )

        AppStart ->
            ( model
            , case model.route of
                Route.NotFound ->
                    [ Outbound.Redirect <| Route.toString Route.New ]

                _ ->
                    [ Outbound.GetUser model.token UserPrepared ]
            )

        UserPrepared token userEntity ->
            ( { model | token = Just token }
            , [ Outbound.AttachToWorkspace token
              , Outbound.SaveToken token
              ]
            )

        WorkspaceAttached packages ->
            case model.route of
                Route.New ->
                    ( { model
                        | workspace =
                            Just
                                { elmCode = ""
                                , htmlCode = ""
                                , packages = packages
                                }
                      }
                    , []
                    )

                Route.Existing revisionId ->
                    ( model
                    , [ Outbound.GetRevision revisionId RevisionReceived ]
                    )

                _ ->
                    ( model, [] )

        RevisionReceived ((Entity id revision) as entity) ->
            ( { model
                | revision = Just entity
                , workspace =
                    Just
                        { elmCode = revision.elmCode
                        , htmlCode = revision.htmlCode
                        , packages = revision.packages
                        }
              }
            , []
            )

        ElmCodeChanged code ->
            ( Model.workspace_ (\w -> { w | elmCode = code }) model
            , []
            )

        HtmlCodeChanged code ->
            ( Model.workspace_ (\w -> { w | htmlCode = code }) model
            , []
            )

        ErrorOccured error ->
            ( model, [] )

        NoOp ->
            ( model, [] )


subscriptions : Model -> List (Inbound Msg)
subscriptions model =
    List.concat
        [ case model.token of
            Just token ->
                [ Inbound.WorkspaceAttached token WorkspaceAttached ]

            Nothing ->
                []
        ]
