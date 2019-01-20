module Pages.Editor.State.App exposing
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
import Pages.Editor.Route as Route exposing (Route(..))
import Pages.Editor.State.Setup as Setup
import Pages.Editor.State.Working as Working
import Pages.Editor.Types.Flags as Flags exposing (Flags)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.User as User exposing (User)


type Model
    = Initial Flags Route
    | Setup Setup.Model
    | Working Working.Model
    | Broken


init : Flags -> Route -> ( Model, Command Msg )
init flags route =
    let
        user =
            Maybe.withDefault User.default flags.user
    in
    Setup.init route user flags.latestTerms
        |> Tuple.mapFirst Setup
        |> Tuple.mapSecond (Command.map SetupMsg)


setupToWorking :
    { token : Jwt
    , recovery : Maybe Revision
    , revision : Revision.External
    , packages : List Package
    , user : User
    }
    -> ( Model, Command Msg )
setupToWorking { token, recovery, revision, packages, user } =
    Working.init token user recovery revision packages
        |> Tuple.mapFirst Working
        |> Tuple.mapSecond (Command.map WorkingMsg)


type Msg
    = NoOp
    | AppStart
    | RouteChanged Route
    | SetupMsg Setup.Msg
    | WorkingMsg Working.Msg


update : Flags -> Msg -> Model -> ( Model, Command Msg )
update flags msg_ model =
    case ( model, msg_ ) of
        ( _, RouteChanged route ) ->
            case model of
                Initial flags _ ->
                    init flags route

                Setup setupState ->
                    update flags (SetupMsg (Setup.RouteChanged route)) model

                _ ->
                    ( model, Command.none )

        ( Setup setupState, SetupMsg msg ) ->
            case Setup.update msg setupState of
                ( Transition.Step next, setupCommand ) ->
                    ( Setup next
                    , Command.map SetupMsg setupCommand
                    )

                ( Transition.Exit data, setupCommand ) ->
                    setupToWorking
                        { token = data.token
                        , recovery = flags.recovery
                        , revision = data.revision
                        , packages = data.packages
                        , user = data.user
                        }

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
