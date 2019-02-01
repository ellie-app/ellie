port module Network.Absinthe.Socket exposing
    ( ChannelStatus(..)
    , Info(..)
    , Logger
    , Msg
    , Socket
    , emptyLogger
    , init
    , listen
    , update
    )

import Extra.Json.Encode as Encode
import Graphql.Document as Document
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Task
import Time


type alias SocketConfig =
    { url : String
    , token : Maybe String
    }


port absintheSocketOutbound : Value -> Cmd msg


socketSend : SocketConfig -> String -> Cmd msg
socketSend socketConfig data =
    absintheSocketOutbound <|
        Encode.object
            [ ( "tag", Encode.string "Send" )
            , ( "url", Encode.string socketConfig.url )
            , ( "token", Encode.maybeNull Encode.string socketConfig.token )
            , ( "data", Encode.string data )
            ]


port absintheSocketInbound : (Value -> msg) -> Sub msg


socketListen : Socket -> Sub Info
socketListen state =
    absintheSocketInbound <|
        \input ->
            case Decode.decodeValue (infoDecoder state) input of
                Ok info ->
                    info

                Err _ ->
                    Control SocketClosed


type alias Socket =
    { connected : Bool
    , channel : ChannelStatus
    , socketConfig : SocketConfig
    , refId : Int
    , docId : Maybe String
    , logger : Logger
    , document : String
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


init : SocketConfig -> Logger -> SelectionSet a RootSubscription -> ( Socket, Cmd msg )
init socketConfig logger doc =
    ( { connected = False
      , channel = Initial
      , socketConfig = socketConfig
      , refId = 0
      , docId = Nothing
      , logger = logger
      , document = Document.serializeSubscription doc
      }
    , absintheSocketOutbound <|
        Encode.object
            [ ( "tag", Encode.string "Initialize" )
            , ( "url", Encode.string socketConfig.url )
            , ( "token", Encode.maybeNull Encode.string socketConfig.token )
            , ( "doc", Encode.string <| Document.serializeSubscription doc )
            ]
    )


type Info
    = Data Value
    | Status Bool
    | Control Msg


type Msg
    = SocketOpened
    | SocketClosed
    | JoinRequested
    | ChannelJoined Int
    | DocRequested
    | DocSubscribed String
    | ProblemResponse String
    | MutationComplete
    | NoOp
    | HeartbeatReceived


update : Msg -> Socket -> ( Socket, Cmd Msg )
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
                    , socketSend state.socketConfig <|
                        Encode.encode 0 <|
                            Encode.list identity
                                [ Encode.null
                                , Encode.string <| String.fromInt (state.refId + 1)
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
                    , socketSend state.socketConfig <|
                        Encode.encode 0 <|
                            Encode.list identity
                                [ Encode.string <| String.fromInt joinRef
                                , Encode.string <| String.fromInt (state.refId + 1)
                                , Encode.string "__absinthe__:control"
                                , Encode.string "doc"
                                , Encode.object [ ( "query", Encode.string state.document ) ]
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

        ProblemResponse string ->
            let
                _ =
                    state.logger "Malformed response" string
            in
            ( state, Cmd.none )

        MutationComplete ->
            ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )

        HeartbeatReceived ->
            ( state, Cmd.none )


listen : Socket -> Sub Info
listen state =
    Sub.batch
        [ Sub.map toStatus (socketListen state)
        , socketListen state
        ]


toStatus : Info -> Info
toStatus socketInfo =
    case socketInfo of
        Control SocketClosed ->
            Status False

        Control (DocSubscribed _) ->
            Status True

        _ ->
            Control NoOp


decodeAbsintheControl : Decoder Msg
decodeAbsintheControl =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\x ->
                case x of
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


infoDecoder : Socket -> Decoder Info
infoDecoder state =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Open" ->
                        Decode.succeed <| Control (DocSubscribed "")

                    "Data" ->
                        Decode.map Data <| Decode.field "data" Decode.value

                    _ ->
                        Decode.succeed <| Control SocketClosed
            )
