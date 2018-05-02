module Pages.Editor.State.App
    exposing
        ( Model(..)
        , Msg(..)
        , init
        , subscriptions
        , update
        )

import Data.Jwt as Jwt exposing (Jwt)
import Data.Transition as Transition exposing (Transition(..))
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Elm.Package as Package exposing (Package)
import Pages.Editor.Effects as Effects
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Route as Route exposing (Route(..))
import Pages.Editor.State.Setup as Setup
import Pages.Editor.State.Working as Working
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.User as User exposing (User)


type Model
    = Initial Flags Route
    | Setup Setup.Model
    | Working Working.Model
    | Broken


init : Flags -> Route -> ( Model, Command Msg )
init flags route =
    case route of
        Route.New ->
            Setup.init flags.token Nothing
                |> Tuple.mapFirst Setup
                |> Tuple.mapSecond (Command.map SetupMsg)

        Route.Existing revisionId ->
            Setup.init flags.token (Just revisionId)
                |> Tuple.mapFirst Setup
                |> Tuple.mapSecond (Command.map SetupMsg)

        NotFound ->
            ( Initial flags route
            , Effects.redirect <| Route.toString Route.New
            )


setupToWorking :
    { token : Jwt
    , revision : Maybe ( RevisionId, Revision )
    , packages : List Package
    , user : User
    }
    -> ( Model, Command Msg )
setupToWorking { token, revision, packages, user } =
    Working.init token user revision packages
        |> Tuple.mapFirst Working
        |> Tuple.mapSecond (Command.map WorkingMsg)


type Msg
    = NoOp
    | AppStart
    | RouteChanged Route
    | SetupMsg Setup.Msg
    | WorkingMsg Working.Msg


update : Msg -> Model -> ( Model, Command Msg )
update msg_ model =
    case ( model, msg_ ) of
        ( _, RouteChanged route ) ->
            case model of
                Initial flags _ ->
                    init flags route

                Setup setupState ->
                    update (SetupMsg (Setup.RouteChanged route)) model

                _ ->
                    ( model, Command.none )

        ( Setup setupState, SetupMsg msg ) ->
            case Setup.update msg setupState of
                ( Transition.Step next, setupCommand ) ->
                    ( Setup next
                    , Command.map SetupMsg setupCommand
                    )

                ( Transition.Exit data, setupCommand ) ->
                    setupToWorking data

        ( Working workingState, WorkingMsg msg ) ->
            Working.update msg workingState
                |> Tuple.mapFirst Working
                |> Tuple.mapSecond (Command.map WorkingMsg)

        _ ->
            ( model, Command.none )


subscriptions : Model -> Subscription Msg
subscriptions state =
    case state of
        Setup setupState ->
            Subscription.map SetupMsg <| Setup.subscriptions setupState

        Working workingState ->
            Subscription.map WorkingMsg <| Working.subscriptions workingState

        _ ->
            Subscription.none
