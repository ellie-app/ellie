module Pages.Editor.State.App
    exposing
        ( Model(..)
        , Msg(..)
        , init
        , subscriptions
        , update
        )

import Data.Entity as Entity exposing (Entity(..))
import Data.Jwt as Jwt exposing (Jwt)
import Data.Transition as Transition exposing (Transition(..))
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.User as User exposing (User)
import Ellie.Types.Workspace as Workspace exposing (Workspace)
import Elm.Package as Package exposing (Package)
import Pages.Editor.Effects.Exception as Exception exposing (Exception)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Route as Route exposing (Route(..))
import Pages.Editor.State.Setup as Setup
import Pages.Editor.State.Working as Working


type Model
    = Initial Flags Route
    | Setup Setup.Model
    | Working Working.Model
    | Broken


init : Flags -> Route -> ( Model, Outbound Msg )
init flags route =
    case route of
        Route.New ->
            Setup.init flags.token Nothing
                |> Tuple.mapFirst Setup
                |> Tuple.mapSecond (Outbound.map SetupMsg)

        Route.Existing revisionId ->
            Setup.init flags.token (Just revisionId)
                |> Tuple.mapFirst Setup
                |> Tuple.mapSecond (Outbound.map SetupMsg)

        NotFound ->
            ( Initial flags route
            , Outbound.Redirect <| Route.toString Route.New
            )


setupToWorking :
    { token : Jwt
    , revision : Maybe (Entity Revision.Id Revision)
    , packages : List Package
    , user : Entity User.Id User
    }
    -> ( Model, Outbound Msg )
setupToWorking { token, revision, packages, user } =
    Working.init token (Entity.record user) revision packages
        |> Tuple.mapFirst Working
        |> Tuple.mapSecond (Outbound.map WorkingMsg)


type Msg
    = NoOp
    | AppStart
    | RouteChanged Route
    | SetupMsg Setup.Msg
    | WorkingMsg Working.Msg
    | ExceptionOccurred Exception


update : Msg -> Model -> ( Model, Outbound Msg )
update msg_ model =
    case ( model, msg_ ) of
        ( _, RouteChanged route ) ->
            case model of
                Initial flags _ ->
                    init flags route

                Setup setupState ->
                    update (SetupMsg (Setup.RouteChanged route)) model

                _ ->
                    ( model, Outbound.none )

        ( Setup setupState, SetupMsg msg ) ->
            case Setup.update msg setupState of
                ( Transition.Step next, setupOutbound ) ->
                    ( Setup next
                    , Outbound.map SetupMsg setupOutbound
                    )

                ( Transition.Exit data, setupOutbound ) ->
                    setupToWorking data

        ( Working workingState, WorkingMsg msg ) ->
            Working.update msg workingState
                |> Tuple.mapFirst Working
                |> Tuple.mapSecond (Outbound.map WorkingMsg)

        ( Working workingState, ExceptionOccurred exception ) ->
            Working.update (Working.ExceptionReceived exception) workingState
                |> Tuple.mapFirst Working
                |> Tuple.mapSecond (Outbound.map WorkingMsg)

        _ ->
            ( model, Outbound.none )


subscriptions : Model -> Inbound Msg
subscriptions state =
    case state of
        Setup setupState ->
            Inbound.map SetupMsg <| Setup.subscriptions setupState

        Working workingState ->
            Inbound.map WorkingMsg <| Working.subscriptions workingState

        _ ->
            Inbound.none
