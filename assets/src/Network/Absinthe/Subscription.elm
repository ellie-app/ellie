module Network.Absinthe.Subscription
    exposing
        ( ChannelStatus(..)
        , Info(..)
        , Logger
        , Msg
        , State
        , emptyLogger
        , init
        , listen
        , update
        )

import Graphqelm.Document as Document
import Graphqelm.Operation exposing (RootSubscription)
import Graphqelm.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Network.Socket as Socket exposing (Info(..))
import Process
import Task
import Time


type alias State a =
    { connected : Bool
    , channel : ChannelStatus
    , url : String
    , refId : Int
    , docId : Maybe String
    , logger : Logger
    , document : SelectionSet a RootSubscription
    }


type ChannelStatus
    = Errored
    | Joined Int
    | Joining Int
    | Initial


type alias Logger =
    String -> String -> String


emptyLogger : Logger
emptyLogger a b =
    b


init : String -> Logger -> SelectionSet a RootSubscription -> State a
init url logger doc =
    { connected = False
    , channel = Initial
    , url = url
    , refId = 0
    , docId = Nothing
    , logger = logger
    , document = doc
    }


type Info a
    = Data a
    | Status Bool
    | Control Msg


type Msg
    = SocketOpened
    | SocketClosed
    | JoinRequested
    | ChannelJoined Int
    | DocRequested
    | DocSubscribed String
    | MutationComplete
    | NoOp
    | ProblemResponse String
    | HeartbeatReceived
    | HeartbeatRequested


update : Msg -> State a -> ( State a, Cmd Msg )
update msg state =
    case msg of
        SocketOpened ->
            ( { state | connected = True, refId = 0, docId = Nothing, channel = Initial }
            , Process.sleep 0
                |> Task.perform (\_ -> JoinRequested)
            )

        SocketClosed ->
            ( { state | connected = False, refId = 0, docId = Nothing, channel = Initial }
            , Cmd.none
            )

        JoinRequested ->
            case ( state.connected, state.channel ) of
                ( _, Joined _ ) ->
                    ( state, Cmd.none )

                ( _, Joining _ ) ->
                    ( state, Cmd.none )

                ( True, _ ) ->
                    ( { state | channel = Joining (state.refId + 1), refId = state.refId + 1 }
                    , Socket.send state.url <|
                        Encode.encode 0 <|
                            -- Encode.object
                            --     [ ( "join_ref", Encode.null )
                            --     , ( "ref", Encode.string <| toString (state.refId + 1) )
                            --     , ( "topic", Encode.string "__absinthe__:control" )
                            --     , ( "event", Encode.string "phx_join" )
                            --     , ( "payload", Encode.object [] )
                            --     ]
                            Encode.list
                                [ Encode.null
                                , Encode.string <| toString (state.refId + 1)
                                , Encode.string "__absinthe__:control"
                                , Encode.string "phx_join"
                                , Encode.object []
                                ]
                    )

                _ ->
                    ( state, Cmd.none )

        ChannelJoined joinRef ->
            case ( state.channel, state.connected ) of
                ( Joining _, True ) ->
                    ( { state | channel = Joined joinRef }
                    , Process.sleep 0
                        |> Task.perform (\_ -> DocRequested)
                    )

                _ ->
                    ( state, Cmd.none )

        DocRequested ->
            case ( state.channel, state.connected, state.docId ) of
                ( Joined joinRef, True, Nothing ) ->
                    ( { state | refId = state.refId + 1 }
                    , Socket.send state.url <|
                        Encode.encode 0 <|
                            -- Encode.object
                            --     [ ( "join_ref", Encode.string <| toString joinRef )
                            --     , ( "ref", Encode.string <| toString (state.refId + 1) )
                            --     , ( "topic", Encode.string "__absinthe__:control" )
                            --     , ( "event", Encode.string "doc" )
                            --     , ( "payload"
                            --       , Encode.object
                            --             [ ( "query"
                            --               , Encode.string <| Document.serializeSubscription state.document
                            --               )
                            --             ]
                            --       )
                            --     ]
                            Encode.list
                                [ Encode.string <| toString joinRef
                                , Encode.string <| toString (state.refId + 1)
                                , Encode.string "__absinthe__:control"
                                , Encode.string "doc"
                                , Encode.object [ ( "query", Encode.string <| Document.serializeSubscription state.document ) ]
                                ]
                    )

                _ ->
                    ( state, Cmd.none )

        DocSubscribed docId ->
            case ( state.channel, state.connected, state.docId ) of
                ( Joined _, True, Nothing ) ->
                    ( { state | docId = Just docId }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        MutationComplete ->
            ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )

        ProblemResponse info ->
            let
                _ =
                    state.logger "problem response" info
            in
            ( state, Cmd.none )

        HeartbeatReceived ->
            ( state, Cmd.none )

        HeartbeatRequested ->
            ( { state | refId = state.refId + 1 }
            , Socket.send state.url <|
                Encode.encode 0 <|
                    -- Encode.object
                    --     [ ( "join_ref", Encode.null )
                    --     , ( "ref", Encode.string <| toString (state.refId + 1) )
                    --     , ( "topic", Encode.string "phoenix" )
                    --     , ( "event", Encode.string "heartbeat" )
                    --     , ( "payload", Encode.object [] )
                    --     ]
                    Encode.list
                        [ Encode.null
                        , Encode.string <| toString (state.refId + 1)
                        , Encode.string "phoenix"
                        , Encode.string "heartbeat"
                        , Encode.object []
                        ]
            )


listen : State a -> Sub (Info a)
listen state =
    Sub.batch
        [ Socket.listen state.url
            |> Sub.map (status state)
        , Socket.listen state.url
            |> Sub.map (listenHelp state)
        , Time.every (30 * Time.second) (\_ -> Control HeartbeatRequested)
        ]


status : State a -> Socket.Info -> Info a
status state socketInfo =
    case listenHelp state socketInfo of
        Control SocketClosed ->
            Status False

        Control (DocSubscribed _) ->
            Status True

        _ ->
            Control NoOp


listenHelp : State a -> Socket.Info -> Info a
listenHelp state socketInfo =
    case socketInfo of
        Socket.Open ->
            Control SocketOpened

        Socket.Close ->
            Control SocketClosed

        Socket.Data data ->
            let
                rawMessage =
                    Decode.decodeValue rawMessageDecoder data
                        |> Result.withDefault nonsenseRawMessage
            in
            case ( state.channel, rawMessage.ref, rawMessage.joinRef, rawMessage.channel, rawMessage.event ) of
                ( Joining refId, Just ref, Just joinRef, "__absinthe__:control", "phx_reply" ) ->
                    Control (ChannelJoined joinRef)

                ( Joined myJoinRef, Just ref, Just joinRef, "__absinthe__:control", "phx_reply" ) ->
                    if myJoinRef == joinRef then
                        case Decode.decodeValue decodeAbsintheControl rawMessage.data of
                            Ok msg ->
                                Control msg

                            Err stuff ->
                                Control (ProblemResponse stuff)
                    else
                        Control NoOp

                ( Joined myJoinRef, Just ref, _, "phoenix", "phx_reply" ) ->
                    Control HeartbeatReceived

                ( Joined myJoinRef, Nothing, Nothing, docId, "subscription:data" ) ->
                    if state.docId == Just docId then
                        let
                            decoder =
                                Document.decoder state.document
                                    |> Decode.field "result"
                        in
                        case Decode.decodeValue decoder rawMessage.data of
                            Ok data ->
                                Data data

                            Err string ->
                                Control (ProblemResponse string)
                    else
                        Control NoOp

                _ ->
                    Control NoOp


decodeAbsintheControl : Decoder Msg
decodeAbsintheControl =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "ok" ->
                        Decode.oneOf
                            [ Decode.field "subscriptionId" Decode.string
                                |> Decode.map DocSubscribed
                            , Decode.field "data" Decode.value
                                |> Decode.map (\_ -> MutationComplete)
                            ]
                            |> Decode.field "response"

                    _ ->
                        Decode.fail "Unsuccessful response"
            )


type alias RawMessage =
    { joinRef : Maybe Int
    , ref : Maybe Int
    , channel : String
    , event : String
    , data : Value
    }


nonsenseRawMessage : RawMessage
nonsenseRawMessage =
    { joinRef = Nothing
    , ref = Nothing
    , channel = "nonsense"
    , event = "failure"
    , data = Encode.null
    }


rawMessageDecoder : Decoder RawMessage
rawMessageDecoder =
    Decode.map5 RawMessage
        (Decode.index 0 (Decode.map (Maybe.andThen (String.toInt >> Result.toMaybe)) (Decode.nullable Decode.string)))
        (Decode.index 1 (Decode.map (Maybe.andThen (String.toInt >> Result.toMaybe)) (Decode.nullable Decode.string)))
        (Decode.index 2 Decode.string)
        (Decode.index 3 Decode.string)
        (Decode.index 4 Decode.value)



-- (Decode.field "join_ref" (Decode.map (Maybe.andThen (String.toInt >> Result.toMaybe)) (Decode.nullable Decode.string)))
-- (Decode.field "ref" (Decode.map (Maybe.andThen (String.toInt >> Result.toMaybe)) (Decode.nullable Decode.string)))
-- (Decode.field "topic" Decode.string)
-- (Decode.field "event" Decode.string)
-- (Decode.field "payload" Decode.value)
