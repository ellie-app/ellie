module Effect.Program exposing (..)

import Css exposing (..)
import Css.Foreign
import Data.Jwt as Jwt
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
import Keyboard
import Murmur3
import Navigation exposing (Location)
import Network.Absinthe.Subscription as Absinthe
import Process
import Task
import Time


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


type Msg msg
    = UserMsg msg
    | SetupSocket String String (SelectionSet msg RootSubscription)
    | SubscriptionMsg String Absinthe.Msg
    | NoOp


type alias State =
    { sockets : Dict String Absinthe.Socket
    }


type Model model msg
    = StartupFailure
    | Running State model


initialState : State
initialState =
    { sockets = Dict.empty
    }


runCmd : Config flags route model msg -> State -> Command msg -> ( State, Cmd (Msg msg) )
runCmd config state cmd =
    case cmd of
        Command.GraphqlQuery url maybeToken selection onError ->
            let
                request =
                    case maybeToken of
                        Just token ->
                            Graphqelm.Http.queryRequestWithHttpGet url Graphqelm.Http.AlwaysGet selection
                                |> Jwt.withTokenHeader token

                        Nothing ->
                            Graphqelm.Http.queryRequestWithHttpGet url Graphqelm.Http.AlwaysGet selection
            in
            ( state
            , Graphqelm.Http.send identity request
                |> Cmd.map (Result.mapError Graphqelm.Http.ignoreParsedErrorData)
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok msg ->
                                UserMsg msg

                            Err str ->
                                UserMsg (onError str)
                    )
            )

        Command.GraphqlMutation url token selection onError ->
            let
                request =
                    case token of
                        Just t ->
                            Graphqelm.Http.mutationRequest url selection
                                |> Jwt.withTokenHeader t

                        Nothing ->
                            Graphqelm.Http.mutationRequest url selection
            in
            ( state
            , Graphqelm.Http.send identity request
                |> Cmd.map (Result.mapError Graphqelm.Http.ignoreParsedErrorData)
                |> Cmd.map
                    (\result ->
                        case result of
                            Ok msg ->
                                UserMsg msg

                            Err str ->
                                UserMsg (onError str)
                    )
            )

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


runSub : Config flags route model msg -> State -> Subscription msg -> Sub (Msg msg)
runSub config state sub =
    case sub of
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
                NoOp ->
                    ( model, Cmd.none )

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
