port module Effect.Program exposing (Config, Model(..), Msg(..), Program, State, debounceConfig, effectProgramKeyDowns, effectProgramTitle, includeTitle, initialState, keyDownDecoder, maybeWithDebounce, maybeWithToken, program, runCmd, runSub, withCaching, wrapInit, wrapSubscriptions, wrapUpdate, wrapView)

import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Global
import Data.Jwt as Jwt exposing (Jwt)
import Debounce
import Dict exposing (Dict)
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Ellie.Ui.Output as Output
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Html
import Html.Styled as Styled
import Json.Decode as Decode exposing (Decoder)
import Murmur3
import Network.Absinthe.Subscription as Absinthe
import Process
import Set exposing (Set)
import Task
import Time
import Url exposing (Url)


port effectProgramKeyDowns : (Decode.Value -> a) -> Sub a


port effectProgramTitle : String -> Cmd a


type alias Config flags route model msg =
    { init : flags -> route -> ( model, Command msg )
    , url : Url -> route
    , route : route -> msg
    , flags : Decoder flags
    , update : flags -> msg -> model -> ( model, Command msg )
    , view : model -> Styled.Html msg
    , title : model -> String
    , styles : List Css.Global.Snippet
    , subscriptions : model -> Subscription msg
    , outbound : ( String, Decode.Value ) -> Cmd (Msg msg)
    , inbound : (( String, Decode.Value ) -> Msg msg) -> Sub (Msg msg)
    }


debounceConfig : (Debounce.Msg -> Msg msg) -> Debounce.Config (Msg msg)
debounceConfig debounceMsg =
    { strategy = Debounce.later 500
    , transform = debounceMsg
    }


type Msg msg
    = UserMsg msg
    | SetupSocket String String (SelectionSet msg RootSubscription)
    | SubscriptionMsg String Absinthe.Msg
    | KeyDown String
    | KeyUp String
    | Debounce String Debounce.Msg
    | NoOp


type alias State msg =
    { navKey : Navigation.Key
    , sockets : Dict String Absinthe.Socket
    , keysDown : Set String
    , debouncers : Dict String (Debounce.Debounce (Cmd (Msg msg)))
    }


type Model flags model msg
    = StartupFailure
    | Running (State msg) flags model


initialState : Navigation.Key -> State msg
initialState navKey =
    { navKey = navKey
    , sockets = Dict.empty
    , keysDown = Set.empty
    , debouncers = Dict.empty
    }


maybeWithToken : Maybe Jwt -> Graphql.Http.Request a -> Graphql.Http.Request a
maybeWithToken maybeToken request =
    case maybeToken of
        Just token ->
            request
                |> Jwt.withTokenHeader token

        Nothing ->
            request


withCaching : Command.CacheLevel -> Graphql.Http.Request a -> Graphql.Http.Request a
withCaching cacheLevel request =
    case cacheLevel of
        Command.Permanent ->
            request
                |> Graphql.Http.withQueryParams [ ( "cache", "permanent" ) ]

        Command.Temporary ->
            request
                |> Graphql.Http.withQueryParams [ ( "cache", "temporary" ) ]

        Command.AlwaysFetch ->
            request


maybeWithDebounce : State msg -> Maybe String -> Cmd (Msg msg) -> ( State msg, Cmd (Msg msg) )
maybeWithDebounce state debounce requestCmd =
    case debounce of
        Just name ->
            state.debouncers
                |> Dict.get name
                |> Maybe.withDefault Debounce.init
                |> Debounce.push (debounceConfig (Debounce name)) requestCmd
                |> Tuple.mapFirst (\newDebouncer -> { state | debouncers = Dict.insert name newDebouncer state.debouncers })

        Nothing ->
            ( state, requestCmd )


runCmd : Config flags route model msg -> State msg -> Command msg -> ( State msg, Cmd (Msg msg) )
runCmd config state cmd =
    case cmd of
        Command.GraphqlQuery { url, token, selection, onError, debounce, cache } ->
            Graphql.Http.queryRequestWithHttpGet url Graphql.Http.AlwaysGet selection
                |> maybeWithToken token
                |> withCaching cache
                |> Graphql.Http.send identity
                |> Cmd.map (Result.mapError Graphql.Http.ignoreParsedErrorData)
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok msg ->
                                UserMsg msg

                            Err str ->
                                UserMsg (onError str)
                    )
                |> maybeWithDebounce state debounce

        Command.GraphqlMutation { url, token, selection, onError, debounce } ->
            Graphql.Http.mutationRequest url selection
                |> maybeWithToken token
                |> Graphql.Http.send identity
                |> Cmd.map (Result.mapError Graphql.Http.ignoreParsedErrorData)
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok msg ->
                                UserMsg msg

                            Err str ->
                                UserMsg (onError str)
                    )
                |> maybeWithDebounce state debounce

        Command.SetTitle title ->
            ( state
            , effectProgramTitle title
            )

        Command.PortSend stuff ->
            config.outbound ( stuff.channel, stuff.data )
                |> maybeWithDebounce state stuff.debounce

        Command.NewUrl url ->
            ( state
            , Navigation.pushUrl state.navKey url
            )

        Command.Redirect url ->
            ( state
            , Navigation.replaceUrl state.navKey url
            )

        Command.Delay millis next ->
            ( state
            , Process.sleep (toFloat millis)
                |> Task.perform (\_ -> UserMsg next)
            )

        Command.ReloadOutput ->
            ( state
            , Output.reload
            )

        Command.Batch commands ->
            commands
                |> List.foldr
                    (\command ( currentState, cmds ) ->
                        let
                            ( nextState, nextCmd ) =
                                runCmd config currentState command
                        in
                        ( nextState, nextCmd :: cmds )
                    )
                    ( state, [] )
                |> Tuple.mapSecond Cmd.batch

        Command.None ->
            ( state, Cmd.none )


keyDownDecoder : Bool -> Bool -> String -> msg -> Decoder (Msg msg)
keyDownDecoder needsShift needsMeta key msg =
    Decode.map3
        (\actualKey shift meta ->
            if shift == needsShift && meta == needsMeta && key == actualKey then
                UserMsg msg

            else
                NoOp
        )
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.map2 (||)
            (Decode.field "metaKey" Decode.bool)
            (Decode.field "ctrlKey" Decode.bool)
        )


runSub : Config flags route model msg -> State msg -> Subscription msg -> Sub (Msg msg)
runSub config state sub =
    case sub of
        Subscription.KeyCombo combo msg ->
            effectProgramKeyDowns
                (\value ->
                    case Decode.decodeValue (keyDownDecoder combo.shift combo.meta combo.key msg) value of
                        Ok wrappedMsg ->
                            wrappedMsg

                        Err _ ->
                            NoOp
                )

        Subscription.PortReceive channel callback ->
            config.inbound <|
                \( inChannel, data ) ->
                    if inChannel == channel then
                        UserMsg (callback data)

                    else
                        NoOp

        Subscription.KeyPress code msg ->
            Browser.Events.onKeyUp <|
                Decode.andThen
                    (\keycode ->
                        Decode.succeed <|
                            if keycode == code then
                                UserMsg msg

                            else
                                NoOp
                    )
                    Decode.int

        Subscription.AbsintheSubscription url selection onStatus ->
            let
                key =
                    url
                        ++ " :: "
                        ++ Graphql.Document.serializeSubscription selection
                        |> Murmur3.hashString 0
                        |> String.fromInt
            in
            case Dict.get key state.sockets of
                Just socket ->
                    Absinthe.listen socket
                        |> Sub.map
                            (\info ->
                                case info of
                                    Absinthe.Data data ->
                                        case Decode.decodeValue (Graphql.Document.decoder selection) data of
                                            Ok msg ->
                                                UserMsg msg

                                            Err _ ->
                                                NoOp

                                    Absinthe.Status bool ->
                                        UserMsg (onStatus bool)

                                    Absinthe.Control msg ->
                                        SubscriptionMsg key msg
                            )

                Nothing ->
                    Time.every 100 (\_ -> SetupSocket key url selection)

        Subscription.Batch subs ->
            Sub.batch <| List.map (runSub config state) subs

        Subscription.None ->
            Sub.none


wrapInit : Config flags route model msg -> Decode.Value -> Url -> Navigation.Key -> ( Model flags model msg, Cmd (Msg msg) )
wrapInit config flagsValue url navKey =
    case Decode.decodeValue config.flags flagsValue of
        Ok flags ->
            let
                ( userModel, userCmd ) =
                    config.init flags (config.url url)

                ( state, cmd ) =
                    runCmd config (initialState navKey) userCmd
            in
            ( Running state flags userModel
            , Cmd.batch [ cmd, effectProgramTitle (config.title userModel) ]
            )

        Err _ ->
            ( StartupFailure
            , Cmd.none
            )


wrapUpdate : Config flags route model msg -> Msg msg -> Model flags model msg -> ( Model flags model msg, Cmd (Msg msg) )
wrapUpdate config msg model =
    includeTitle config.title <|
        case model of
            StartupFailure ->
                ( model, Cmd.none )

            Running state flags m ->
                case msg of
                    Debounce name debounceMsg ->
                        state.debouncers
                            |> Dict.get name
                            |> Maybe.map (Debounce.update (debounceConfig (Debounce name)) (Debounce.takeLast identity) debounceMsg)
                            |> Maybe.map (Tuple.mapFirst (\debouncer -> Running { state | debouncers = Dict.insert name debouncer state.debouncers } flags m))
                            |> Maybe.withDefault ( model, Cmd.none )

                    NoOp ->
                        ( model, Cmd.none )

                    KeyDown key ->
                        ( Running { state | keysDown = Set.insert key state.keysDown } flags m
                        , Cmd.none
                        )

                    KeyUp key ->
                        ( Running { state | keysDown = Set.remove key state.keysDown } flags m
                        , Cmd.none
                        )

                    UserMsg userMsg ->
                        let
                            ( userModel, userCmd ) =
                                config.update flags userMsg m

                            ( newState, cmd ) =
                                runCmd config state userCmd
                        in
                        ( Running newState flags userModel
                        , cmd
                        )

                    SetupSocket key url selection ->
                        ( Running
                            { state | sockets = Dict.insert key (Absinthe.init url Debug.log selection) state.sockets }
                            flags
                            m
                        , Cmd.none
                        )

                    SubscriptionMsg key socketMsg ->
                        case Dict.get key state.sockets of
                            Just socket ->
                                let
                                    ( newSocket, socketCmd ) =
                                        Absinthe.update socketMsg socket
                                in
                                ( Running
                                    { state | sockets = Dict.insert key newSocket state.sockets }
                                    flags
                                    m
                                , Cmd.map (SubscriptionMsg key) socketCmd
                                )

                            Nothing ->
                                ( model, Cmd.none )


includeTitle : (model -> String) -> ( Model flags model msg, Cmd (Msg msg) ) -> ( Model flags model msg, Cmd (Msg msg) )
includeTitle produceTitle ( model, cmd ) =
    case model of
        StartupFailure ->
            ( model, cmd )

        Running _ _ m ->
            ( model
            , Cmd.batch [ cmd, effectProgramTitle (produceTitle m) ]
            )


wrapView : Config flags route model msg -> Model flags model msg -> Browser.Document (Msg msg)
wrapView config model =
    { title = "Ellie"
    , body =
        [ case model of
            StartupFailure ->
                Html.text "Couldn't decode the flags"

            Running _ _ m ->
                Styled.toUnstyled <|
                    Styled.styled Styled.div
                        [ height (pct 100) ]
                        []
                        [ Css.Global.global config.styles
                        , Styled.map UserMsg <| config.view m
                        , Styled.node "ellie-ui-portal" [] []
                        ]
        ]
    }


wrapSubscriptions : Config flags route model msg -> Model flags model msg -> Sub (Msg msg)
wrapSubscriptions config model =
    case model of
        StartupFailure ->
            Sub.none

        Running state _ m ->
            runSub config state (config.subscriptions m)


type alias Program flags model msg =
    Platform.Program Decode.Value (Model flags model msg) (Msg msg)


program : Config flags route model msg -> Program flags model msg
program config =
    Browser.application
        { init = wrapInit config
        , update = wrapUpdate config
        , view = wrapView config
        , subscriptions = wrapSubscriptions config
        , onUrlChange = UserMsg << config.route << config.url
        , onUrlRequest = \_ -> NoOp
        }
