port module Pages.Editor.Effects.State
    exposing
        ( Config
        , Msg(..)
        , State
        , init
        , subscriptions
        , update
        )

import Data.Jwt as Jwt exposing (Jwt)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Elm.Docs as Docs exposing (Module)
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Extra.Json.Encode as Encode
import Extra.Result as Result
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keyboard
import Navigation
import Network.Absinthe.Subscription as Subscription
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Handlers as Handlers
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound(..))
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound(..))
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)
import Process
import Task
import Time


port pagesEditorEffectsStateIn : (Value -> msg) -> Sub msg


port pagesEditorEffectsStateOut : Value -> Cmd msg


processOutbound : (Exception -> msg) -> Outbound msg -> State model msg -> ( State model msg, Cmd (Msg msg) )
processOutbound onError effect state =
    case effect of
        GetDocs packages callback ->
            ( state
            , Handlers.getDocs packages
                |> Cmd.map (callback >> UserMsg)
            )

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

        AttachToWorkspace token version ->
            ( state
            , Handlers.attachToWorkspace token version
                |> Cmd.map (\_ -> NoOp)
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

        FormatElmCode version code callback ->
            ( state
            , Handlers.formatCode version code
                |> Cmd.map (Result.mapError Exception.fromGqlError)
                |> Cmd.map (Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        Compile token version elm packages ->
            ( state
            , Handlers.compile token version elm packages
                |> Cmd.map (\_ -> NoOp)
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
    case inbound of
        EscapePressed next ->
            Keyboard.ups
                (\keycode ->
                    if keycode == 27 then
                        UserMsg next
                    else
                        NoOp
                )

        WorkspaceUpdates token callback ->
            case state.socket of
                Nothing ->
                    Time.every 100 (\_ -> SetupSocket token)

                Just socket ->
                    Subscription.listen socket
                        |> Sub.map
                            (\info ->
                                case info of
                                    Subscription.Data data ->
                                        UserMsg (callback data)

                                    Subscription.Control msg ->
                                        SubscriptionMsg msg

                                    Subscription.Status True ->
                                        UserMsg (callback WorkspaceUpdate.Connected)

                                    Subscription.Status False ->
                                        UserMsg (callback WorkspaceUpdate.Disconnected)
                            )

        OutputThrewException callback ->
            Decode.string
                |> Decode.map (callback >> UserMsg)
                |> portDecoder "OutputThrewException"
                |> pagesEditorEffectsStateIn

        Inbound.Batch inbounds ->
            Sub.batch <| List.map (processInbound state) inbounds

        Inbound.None ->
            Sub.none


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
    | SetupSocket Jwt
    | SubscriptionMsg Subscription.Msg
    | DebouncePackageSearch Debounce.Msg
    | SaveSettingsSpawned Process.Id
    | DebounceSaveSettings Debounce.Msg
    | DocsReceived (List Package) (List Module -> msg) (Result Http.Error (Dict String (List Module)))
    | ExceptionOccured Exception
    | Multiple (List (Msg msg))
    | NoOp


type alias State model msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    , getUserTagger : Maybe (User -> msg)
    , saveSettingsId : Maybe Process.Id
    , debounceSaveSettings : Debounce (Cmd (Msg msg))
    , docsCache : Dict String (List Module)
    , socket : Maybe (Subscription.State WorkspaceUpdate)
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

        initialState =
            { debouncePackageSearch = Debounce.init
            , getUserTagger = Nothing
            , saveSettingsId = Nothing
            , debounceSaveSettings = Debounce.init
            , docsCache = Dict.empty
            , socket = Nothing
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
update ({ onError, userUpdate } as config) msg state =
    case msg of
        SetupSocket token ->
            ( { state | socket = Just <| Handlers.setupSocket token }
            , Cmd.none
            )

        SubscriptionMsg subMsg ->
            case state.socket of
                Nothing ->
                    ( state, Cmd.none )

                Just socket ->
                    let
                        ( newSocket, socketCmd ) =
                            Subscription.update subMsg socket
                    in
                    ( { state | socket = Just newSocket }
                    , Cmd.map SubscriptionMsg socketCmd
                    )

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
