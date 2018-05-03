port module Effect.Program exposing (..)

import Css exposing (..)
import Css.Foreign
import Data.Jwt as Jwt exposing (Jwt)
import Debounce
import Dict exposing (Dict)
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Ellie.Ui.Output as Output
import Graphqelm.Document
import Graphqelm.Http
import Graphqelm.Operation exposing (RootSubscription)
import Graphqelm.SelectionSet exposing (SelectionSet)
import Html
import Html.Styled as Styled
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (KeyCode)
import Murmur3
import Navigation exposing (Location)
import Network.Absinthe.Subscription as Absinthe
import Process
import Set exposing (Set)
import Task
import Time


port effectProgramKeyDowns : (Decode.Value -> a) -> Sub a


type alias Config flags route model msg =
    { init : flags -> route -> ( model, Command msg )
    , url : Location -> route
    , route : route -> msg
    , flags : Decoder flags
    , update : msg -> model -> ( model, Command msg )
    , view : model -> Styled.Html msg
    , styles : List Css.Foreign.Snippet
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
    { sockets : Dict String Absinthe.Socket
    , keysDown : Set String
    , debouncers : Dict String (Debounce.Debounce (Cmd (Msg msg)))
    }


type Model model msg
    = StartupFailure
    | Running (State msg) model


initialState : State msg
initialState =
    { sockets = Dict.empty
    , keysDown = Set.empty
    , debouncers = Dict.empty
    }


maybeWithToken : Maybe Jwt -> Graphqelm.Http.Request a -> Graphqelm.Http.Request a
maybeWithToken maybeToken request =
    case maybeToken of
        Just token ->
            request
                |> Jwt.withTokenHeader token

        Nothing ->
            request


withCaching : Command.CacheLevel -> Graphqelm.Http.Request a -> Graphqelm.Http.Request a
withCaching cacheLevel request =
    case cacheLevel of
        Command.Permanent ->
            request
                |> Graphqelm.Http.withQueryParams [ ( "cache", "permanent" ) ]

        Command.Temporary ->
            request
                |> Graphqelm.Http.withQueryParams [ ( "cache", "temporary" ) ]

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
            Graphqelm.Http.queryRequestWithHttpGet url Graphqelm.Http.AlwaysGet selection
                |> maybeWithToken token
                |> withCaching cache
                |> Graphqelm.Http.send identity
                |> Cmd.map (Result.mapError Graphqelm.Http.ignoreParsedErrorData)
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
            Graphqelm.Http.mutationRequest url selection
                |> maybeWithToken token
                |> Graphqelm.Http.send identity
                |> Cmd.map (Result.mapError Graphqelm.Http.ignoreParsedErrorData)
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok msg ->
                                UserMsg msg

                            Err str ->
                                UserMsg (onError str)
                    )
                |> maybeWithDebounce state debounce

        Command.PortSend channel data ->
            ( state
            , config.outbound ( channel, data )
            )

        Command.NewUrl url ->
            ( state
            , Navigation.newUrl url
            )

        Command.Redirect url ->
            ( state
            , Navigation.modifyUrl url
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
                    (\command ( state, cmds ) ->
                        let
                            ( nextState, cmd ) =
                                runCmd config state command
                        in
                        ( nextState, cmd :: cmds )
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
                        Ok msg ->
                            msg

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
            Keyboard.ups
                (\keycode ->
                    if keycode == code then
                        UserMsg msg
                    else
                        NoOp
                )

        Subscription.AbsintheSubscription url selection onStatus ->
            let
                key =
                    url
                        ++ " :: "
                        ++ Graphqelm.Document.serializeSubscription selection
                        |> Murmur3.hashString 0
                        |> toString
            in
            case Dict.get key state.sockets of
                Just socket ->
                    Absinthe.listen socket
                        |> Sub.map
                            (\info ->
                                case info of
                                    Absinthe.Data data ->
                                        case Decode.decodeValue (Graphqelm.Document.decoder selection) data of
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


wrapInit : Config flags route model msg -> Decode.Value -> Location -> ( Model model msg, Cmd (Msg msg) )
wrapInit config flagsValue location =
    case Decode.decodeValue config.flags flagsValue of
        Ok flags ->
            let
                ( userModel, userCmd ) =
                    config.init flags (config.url location)

                ( state, cmd ) =
                    runCmd config initialState userCmd
            in
            ( Running state userModel
            , cmd
            )

        Err _ ->
            ( StartupFailure
            , Cmd.none
            )


wrapUpdate : Config flags route model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate config msg model =
    case model of
        StartupFailure ->
            ( model, Cmd.none )

        Running state m ->
            case msg of
                Debounce name debounceMsg ->
                    state.debouncers
                        |> Dict.get name
                        |> Maybe.map (Debounce.update (debounceConfig (Debounce name)) (Debounce.takeLast identity) debounceMsg)
                        |> Maybe.map (Tuple.mapFirst (\debouncer -> Running { state | debouncers = Dict.insert name debouncer state.debouncers } m))
                        |> Maybe.withDefault ( model, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

                KeyDown key ->
                    ( Running { state | keysDown = Set.insert key state.keysDown } m
                    , Cmd.none
                    )

                KeyUp key ->
                    ( Running { state | keysDown = Set.remove key state.keysDown } m
                    , Cmd.none
                    )

                UserMsg userMsg ->
                    let
                        ( userModel, userCmd ) =
                            config.update userMsg m

                        ( newState, cmd ) =
                            runCmd config state userCmd
                    in
                    ( Running newState userModel
                    , cmd
                    )

                SetupSocket key url selection ->
                    ( Running
                        { state | sockets = Dict.insert key (Absinthe.init url Debug.log selection) state.sockets }
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
                                m
                            , Cmd.map (SubscriptionMsg key) socketCmd
                            )

                        Nothing ->
                            ( model, Cmd.none )


wrapView : Config flags route model msg -> Model model msg -> Html.Html (Msg msg)
wrapView config model =
    case model of
        StartupFailure ->
            Html.text ""

        Running _ m ->
            Styled.toUnstyled <|
                Styled.styled Styled.div
                    [ height (pct 100) ]
                    []
                    [ Css.Foreign.global config.styles
                    , Styled.map UserMsg <| config.view m
                    , Styled.node "ellie-ui-portal" [] []
                    ]


wrapSubscriptions : Config flags route model msg -> Model model msg -> Sub (Msg msg)
wrapSubscriptions config model =
    case model of
        StartupFailure ->
            Sub.none

        Running state m ->
            runSub config state (config.subscriptions m)


type alias Program model msg =
    Platform.Program Decode.Value (Model model msg) (Msg msg)


program : Config flags route model msg -> Program model msg
program config =
    Navigation.programWithFlags (config.url >> config.route >> UserMsg)
        { init = wrapInit config
        , update = wrapUpdate config
        , view = wrapView config
        , subscriptions = wrapSubscriptions config
        }
