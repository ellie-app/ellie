module Pages.Embed.State.App exposing (..)

import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Elm.Error as Error exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Pages.Embed.Effects as Effects
import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.Panel as Panel exposing (Panel)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Embed.Types.Route as Route exposing (Route)


type OutputState
    = NotRun
    | AcquiringConnection
    | Compiling
    | Finished (Maybe Error)
    | Crashed String


type DebugState
    = DebuggerUnavailable
    | Debugging
    | NotDebugging


type alias WorkingState =
    { panel : Panel
    , revision : { id : RevisionId, data : Revision }
    , output : OutputState
    , debug : DebugState
    }


type Model
    = Failure
    | Loading RevisionId Panel
    | Working WorkingState


type alias Flags =
    {}


flags : Decoder Flags
flags =
    Decode.succeed {}


init : Flags -> Route -> ( Model, Command Msg )
init flags route =
    case route of
        Route.NotFound ->
            ( Failure, Command.none )

        Route.Existing revisionId panel ->
            ( Loading revisionId panel
            , Effects.getRevision revisionId
                |> Command.map (Result.mapError (\_ -> ()))
                |> Command.map (RevisionLoaded revisionId)
            )


type Msg
    = RevisionLoaded RevisionId (Result () Revision)
    | RouteChanged Route
    | UpdateReceived EmbedUpdate
    | EmbedRunStarted
    | RunCompleted (Result () (Maybe (Maybe Error)))
    | CanDebugChanged Bool
    | ToggleDebugger Bool
    | PanelSelected Panel
    | GoToPosition Error.Position
    | ReloadOutput


update : Flags -> Msg -> Model -> ( Model, Command Msg )
update flags msg model =
    case ( model, msg ) of
        ( _, RouteChanged route ) ->
            init {} route

        ( Failure, _ ) ->
            ( model, Command.none )

        ( Loading rid panel, RevisionLoaded revisionId (Ok revision) ) ->
            if RevisionId.eq rid revisionId then
                ( Working
                    { panel = panel
                    , revision = { id = revisionId, data = revision }
                    , output = NotRun
                    , debug = DebuggerUnavailable
                    }
                , Command.none
                )
            else
                ( model, Command.none )

        ( Loading rid panel, RevisionLoaded revisionId (Err error) ) ->
            if RevisionId.eq rid revisionId then
                ( Failure, Command.none )
            else
                ( model, Command.none )

        ( Working state, ReloadOutput ) ->
            ( model
            , Command.ReloadOutput
            )

        ( Working state, CanDebugChanged canDebug ) ->
            ( Working
                { state
                    | debug =
                        if canDebug then
                            NotDebugging
                        else
                            DebuggerUnavailable
                }
            , Command.none
            )

        ( Working state, ToggleDebugger open ) ->
            ( Working
                { state
                    | debug =
                        case ( open, state.debug ) of
                            ( False, Debugging ) ->
                                NotDebugging

                            ( True, NotDebugging ) ->
                                Debugging

                            _ ->
                                state.debug
                }
            , Command.none
            )

        ( Working state, PanelSelected panel ) ->
            ( Working { state | panel = panel }
            , Command.none
            )

        ( Working state, EmbedRunStarted ) ->
            ( Working { state | output = AcquiringConnection }
            , Command.none
            )

        ( Working state, RunCompleted (Ok (Just error)) ) ->
            ( Working { state | output = Finished error }
            , Command.none
            )

        ( Working state, RunCompleted (Ok Nothing) ) ->
            ( Working state, Command.none )

        ( Working state, RunCompleted (Err _) ) ->
            ( Working
                { state
                    | output = Crashed "Compiler process failed to start. Sometimes this is because of a network connection error. If you are online, though, this was a server failure and it has been automatically reported."
                }
            , Command.none
            )

        ( Working state, UpdateReceived embedUpdate ) ->
            case ( state.output, embedUpdate ) of
                ( AcquiringConnection, EmbedUpdate.Connected ) ->
                    ( Working { state | output = Compiling }
                    , Effects.runEmbed state.revision.id
                        |> Command.map (Result.mapError (\_ -> ()))
                        |> Command.map RunCompleted
                    )

                ( Compiling, EmbedUpdate.Compiled error ) ->
                    ( Working { state | output = Finished error }
                    , Command.none
                    )

                ( Compiling, EmbedUpdate.Failed message ) ->
                    ( Working { state | output = Crashed message }
                    , Command.none
                    )

                ( _, EmbedUpdate.Disconnected ) ->
                    ( Working { state | output = NotRun }
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        ( Working state, GoToPosition position ) ->
            ( Working { state | panel = Panel.Elm }
            , Effects.goToPosition position
            )

        _ ->
            ( model, Command.none )


subscriptions : Model -> Subscription Msg
subscriptions model =
    case model of
        Working state ->
            Subscription.batch
                [ case state.output of
                    AcquiringConnection ->
                        Effects.embedUpdates state.revision.id
                            |> Subscription.map UpdateReceived

                    Compiling ->
                        Effects.embedUpdates state.revision.id
                            |> Subscription.map UpdateReceived

                    _ ->
                        Subscription.none
                ]

        _ ->
            Subscription.none
