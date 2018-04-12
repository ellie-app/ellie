port module Pages.Editor.Effects.State
    exposing
        ( Config
        , Msg(..)
        , State
        , init
        , subscriptions
        , update
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Ellie.Api.Union as ApiUnion
import Ellie.Constants as Constants
import Elm.Docs as Docs exposing (Module)
import Elm.Error as ElmError
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Extra.HttpBuilder exposing (withMaybe)
import Extra.Json.Encode as Encode
import Extra.Result as Result
import Extra.String as String
import Graphqelm.Subscription as Subscription
import Http
import Http.Extra as Http
import HttpBuilder exposing (get, post, put, withBearerToken, withCredentials, withExpect, withHeader, withJsonBody, withQueryParams, withStringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keyboard
import Navigation
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Handlers as Handlers
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound(..))
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound(..))
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.Settings as Settings exposing (Settings)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)
import Process
import Task


port pagesEditorEffectsStateIn : (Value -> msg) -> Sub msg


port pagesEditorEffectsStateOut : Value -> Cmd msg


processOutbound : (Exception -> msg) -> Outbound msg -> State model msg -> ( State model msg, Cmd (Msg msg) )
processOutbound onError effect state =
    case effect of
        Navigate url ->
            ( state
            , Navigation.newUrl url
            )

        GetRevision id callback ->
            ( state
            , Handlers.getRevision id
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        CreateRevision token revision callback ->
            ( state
            , Handlers.createRevision token revision
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold (\rid -> callback rid revision) onError)
                |> Cmd.map UserMsg
            )

        UpdateRevision token projectId revision callback ->
            ( state
            , Handlers.updateRevision token projectId revision
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold (\rid -> callback rid revision) onError)
                |> Cmd.map UserMsg
            )

        MoveElmCursor position ->
            ( state
            , Encode.genericUnion "MoveElmCursor"
                [ Encode.int position.line
                , Encode.int position.column
                ]
                |> pagesEditorEffectsStateOut
            )

        DownloadZip { project, elm, html } ->
            ( state
            , Encode.genericUnion "DownloadZip"
                [ Encode.string <| Encode.encode 2 (Project.encoder project)
                , Encode.string elm
                , Encode.string html
                ]
                |> pagesEditorEffectsStateOut
            )

        OpenInNewTab url ->
            ( state
            , Encode.genericUnion "OpenInNewTab"
                [ Encode.string url ]
                |> pagesEditorEffectsStateOut
            )

        AttachToWorkspace token ->
            let
                ( socket, cmd ) =
                    Handlers.setupSocket token
            in
            ( { state
                | socket = Just ( Subscription.Uninitialized, socket )
              }
            , Cmd.map (subInfoToMsg state.socketCallbacks) cmd
            )

        CreateGist stuff callback ->
            ( state
            , Handlers.createGist stuff.title stuff.elm stuff.html stuff.project
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok url ->
                                UserMsg <| callback (Just url)

                            Err error ->
                                Multiple
                                    [ UserMsg <| onError <| Exception.fromHttp error
                                    , UserMsg <| callback Nothing
                                    ]
                    )
            )

        SaveSettings token settings ->
            state.saveSettingsId
                |> Maybe.map Process.kill
                |> Maybe.withDefault (Task.succeed ())
                |> Task.andThen (\_ -> Process.spawn (Handlers.updateSettings token settings))
                |> Task.perform SaveSettingsSpawned
                |> (\cmd -> Debounce.push debounceSaveSettingsConfig cmd state.debounceSaveSettings)
                |> Tuple.mapFirst (\d -> { state | debounceSaveSettings = d })

        Delay time callback ->
            ( state
            , Process.sleep time
                |> Task.perform (\_ -> UserMsg callback)
            )

        EnableNavigationCheck enabled ->
            ( state
            , Encode.genericUnion "EnableNavigationCheck"
                [ Encode.bool enabled ]
                |> pagesEditorEffectsStateOut
            )

        SearchPackages query callback ->
            Handlers.searchPackages query
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok packages ->
                                UserMsg <| callback (Just packages)

                            Err error ->
                                Multiple
                                    [ UserMsg <| onError <| Exception.fromGqlError error
                                    , UserMsg <| callback Nothing
                                    ]
                    )
                |> (\cmd -> Debounce.push debouncePackageSearchConfig cmd state.debouncePackageSearch)
                |> Tuple.mapFirst (\d -> { state | debouncePackageSearch = d })

        SaveToken token ->
            ( state
            , Encode.genericUnion "SaveToken"
                [ Jwt.encoder token ]
                |> pagesEditorEffectsStateOut
            )

        Authenticate maybeToken callback ->
            ( state
            , Handlers.authenticate maybeToken
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold (\( a, b, c ) -> callback a b c) onError)
                |> Cmd.map UserMsg
            )

        AcceptTerms token termsVersion msg ->
            ( state
            , Handlers.acceptTerms token termsVersion
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold (\_ -> msg) onError)
                |> Cmd.map UserMsg
            )

        FormatElmCode code callback ->
            ( state
            , Handlers.formatCode code
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        Compile elm packages ->
            ( state
            , case state.socket of
                Just ( Subscription.Connected, socket ) ->
                    Handlers.compile socket elm packages
                        |> Cmd.map (\_ -> NoOp)

                _ ->
                    Cmd.none
            )

        ReloadIframe ->
            ( state
            , Encode.genericUnion "ReloadIframe" []
                |> pagesEditorEffectsStateOut
            )

        Redirect url ->
            ( state
            , Navigation.modifyUrl url
            )

        SwitchToDebugger ->
            ( state
            , Encode.genericUnion "SwitchToDebugger" []
                |> pagesEditorEffectsStateOut
            )

        SwitchToProgram ->
            ( state
            , Encode.genericUnion "SwitchToProgram" []
                |> pagesEditorEffectsStateOut
            )

        Outbound.Batch outbounds ->
            outbounds
                |> List.foldr
                    (\outbound ( state, cmds ) ->
                        let
                            ( nextState, cmd ) =
                                processOutbound onError outbound state
                        in
                        ( nextState, cmd :: cmds )
                    )
                    ( state, [] )
                |> Tuple.mapSecond Cmd.batch

        Outbound.None ->
            ( state, Cmd.none )


processInbound : State model msg -> Inbound msg -> Sub (Msg msg)
processInbound state inbound =
    Sub.batch
        [ case ( state.socketCallbacks, state.socket ) of
            ( [], _ ) ->
                Sub.none

            ( _, Nothing ) ->
                Sub.none

            ( callbacks, Just ( _, socket ) ) ->
                Subscription.listen Handlers.Control socket
                    |> Sub.map (subInfoToMsg callbacks)
        , case inbound of
            EscapePressed next ->
                Keyboard.ups
                    (\keycode ->
                        if keycode == 27 then
                            UserMsg next
                        else
                            NoOp
                    )

            WorkspaceUpdates callback ->
                Sub.none

            OutputThrewException callback ->
                Decode.string
                    |> Decode.map (callback >> UserMsg)
                    |> portDecoder "OutputThrewException"
                    |> pagesEditorEffectsStateIn

            Inbound.Batch inbounds ->
                Sub.batch <| List.map (processInbound state) inbounds

            Inbound.None ->
                Sub.none
        ]


portDecoder : String -> Decoder (Msg msg) -> Value -> Msg msg
portDecoder topic decoder message =
    let
        decoder =
            Decode.field "topic" Decode.string
                |> Decode.andThen
                    (\realTopic ->
                        if realTopic == topic then
                            Decode.oneOf
                                [ Decode.map ExceptionOccured <| Decode.field "exception" Exception.decoder
                                , Decode.field "contents" decoder
                                ]
                        else
                            Decode.succeed NoOp
                    )
    in
    message
        |> Decode.decodeValue decoder
        |> Result.fold identity (ClientDecoderFailure >> ExceptionOccured)



----


type Msg msg
    = UserMsg msg
    | DebouncePackageSearch Debounce.Msg
    | SaveSettingsSpawned Process.Id
    | DebounceSaveSettings Debounce.Msg
    | DocsReceived (List Package) (List Module -> msg) (Result Http.Error (Dict String (List Module)))
    | ExceptionOccured Exception
    | Multiple (List (Msg msg))
    | SubscriptionMsg (Subscription.Msg WorkspaceUpdate)
    | SubscriptionStatusChange Subscription.Status
    | NoOp


type alias State model msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    , getUserTagger : Maybe (User -> msg)
    , saveSettingsId : Maybe Process.Id
    , debounceSaveSettings : Debounce (Cmd (Msg msg))
    , docsCache : Dict String (List Module)
    , socket : Maybe ( Subscription.Status, Subscription.Model Handlers.SubscriptionInfo WorkspaceUpdate )
    , socketCallbacks : List (WorkspaceUpdate -> msg)
    , model : model
    }


type alias Config msg flags route model =
    { onError : Exception -> msg
    , userUpdate : msg -> model -> ( model, Outbound msg )
    , userInit : flags -> route -> ( model, Outbound msg )
    , userSubs : model -> Inbound msg
    }


subscriptions : Config msg flags route model -> State model msg -> Sub (Msg msg)
subscriptions config state =
    processInbound state <| config.userSubs state.model


init :
    Config msg flags route model
    -> flags
    -> route
    -> ( State model msg, Cmd (Msg msg) )
init { onError, userInit, userSubs } flags route =
    let
        ( model, outbound ) =
            userInit flags route

        socketCallbacks =
            model
                |> userSubs
                |> Inbound.flatten
                |> List.filterMap
                    (\inbound ->
                        case inbound of
                            WorkspaceUpdates callback ->
                                Just callback

                            _ ->
                                Nothing
                    )

        initialState =
            { debouncePackageSearch = Debounce.init
            , getUserTagger = Nothing
            , saveSettingsId = Nothing
            , debounceSaveSettings = Debounce.init
            , docsCache = Dict.empty
            , socket = Nothing
            , socketCallbacks = socketCallbacks
            , model = model
            }

        ( state, cmd ) =
            processOutbound onError outbound initialState
    in
    ( state
    , cmd
    )


update :
    Config msg flags route model
    -> Msg msg
    -> State model msg
    -> ( State model msg, Cmd (Msg msg) )
update ({ onError, userUpdate } as config) msg beforeSocketState =
    let
        state =
            { beforeSocketState | socketCallbacks = isolateSocketCallbacks config beforeSocketState }
    in
    case msg of
        SubscriptionMsg subMsg ->
            case state.socket of
                Just ( status, socket ) ->
                    let
                        ( newSocket, newCmd ) =
                            Subscription.update subMsg socket
                    in
                    ( { state | socket = Just ( status, newSocket ) }
                    , newCmd
                        |> Cmd.map (subInfoToMsg state.socketCallbacks)
                    )

                Nothing ->
                    ( state, Cmd.none )

        SubscriptionStatusChange status ->
            case ( status, state.socket ) of
                ( Subscription.Connected, Just ( Subscription.Uninitialized, socket ) ) ->
                    ( { state | socket = Just ( status, socket ) }
                    , Handlers.attachToWorkspace socket
                        |> Cmd.map (subInfoToMsg state.socketCallbacks)
                    )

                ( _, Just ( _, socket ) ) ->
                    ( { state | socket = Just ( status, socket ) }
                    , Cmd.none
                    )

                ( _, Nothing ) ->
                    ( state, Cmd.none )

        DocsReceived packages callback cacheResult ->
            case cacheResult of
                Ok cache ->
                    packages
                        |> List.filterMap (\p -> Dict.get (Package.toString p) cache)
                        |> List.concat
                        |> callback
                        |> UserMsg
                        |> (\m -> update config m { state | docsCache = cache })

                Err _ ->
                    ( state, Cmd.none )

        Multiple msgs ->
            List.foldl
                (\nextMsg ( model, cmds ) ->
                    let
                        ( nextModel, nextCmd ) =
                            update config nextMsg model
                    in
                    ( nextModel, nextCmd :: cmds )
                )
                ( state, [] )
                msgs
                |> Tuple.mapSecond Cmd.batch

        UserMsg userMsg ->
            let
                ( nextModel, outbound ) =
                    userUpdate userMsg state.model

                ( nextState, cmd ) =
                    processOutbound onError outbound state
            in
            ( { nextState | model = nextModel }, cmd )

        DebouncePackageSearch msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debouncePackageSearchConfig
                        (Debounce.takeLast identity)
                        msg
                        state.debouncePackageSearch
            in
            ( { state | debouncePackageSearch = debounce }
            , cmd
            )

        SaveSettingsSpawned pid ->
            ( { state | saveSettingsId = Just pid }
            , Cmd.none
            )

        DebounceSaveSettings msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceSaveSettingsConfig
                        (Debounce.takeLast identity)
                        msg
                        state.debounceSaveSettings
            in
            ( { state | debounceSaveSettings = debounce }
            , cmd
            )

        ExceptionOccured exception ->
            let
                ( nextModel, outbound ) =
                    userUpdate (onError exception) state.model

                ( nextState, cmd ) =
                    processOutbound onError outbound { state | model = nextModel }
            in
            ( nextState, cmd )

        NoOp ->
            ( state, Cmd.none )


isolateSocketCallbacks : Config msg flags route model -> State model msg -> List (WorkspaceUpdate -> msg)
isolateSocketCallbacks { userSubs } { model } =
    model
        |> userSubs
        |> Inbound.flatten
        |> List.filterMap
            (\inbound ->
                case inbound of
                    WorkspaceUpdates callback ->
                        Just callback

                    _ ->
                        Nothing
            )


subInfoToMsg : List (WorkspaceUpdate -> msg) -> Handlers.SubscriptionInfo -> Msg msg
subInfoToMsg callbacks info =
    case info of
        Handlers.Data data ->
            callbacks
                |> List.map (\cb -> UserMsg (cb data))
                |> Multiple

        Handlers.Control msg ->
            SubscriptionMsg msg

        Handlers.Status status ->
            SubscriptionStatusChange status


debouncePackageSearchConfig : Debounce.Config (Msg msg)
debouncePackageSearchConfig =
    { strategy = Debounce.later 500
    , transform = DebouncePackageSearch
    }


debounceSaveSettingsConfig : Debounce.Config (Msg msg)
debounceSaveSettingsConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceSaveSettings
    }
