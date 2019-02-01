port module Network.Absinthe.Socket exposing
    ( Info(..)
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


port absintheSocketOutbound : Value -> Cmd msg


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
    , url : String
    , token : Maybe String
    , logger : Logger
    , document : String
    }


type alias Logger =
    String -> String -> String


emptyLogger : Logger
emptyLogger a b =
    b


init :
    { url : String, token : Maybe String }
    -> Logger
    -> SelectionSet a RootSubscription
    -> ( Socket, Cmd msg )
init { url, token } logger doc =
    ( { connected = False
      , url = url
      , token = token
      , logger = logger
      , document = Document.serializeSubscription doc
      }
    , absintheSocketOutbound <|
        Encode.object
            [ ( "tag", Encode.string "Initialize" )
            , ( "url", Encode.string url )
            , ( "token", Encode.maybeNull Encode.string token )
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
    | ProblemResponse String
    | NoOp


update : Msg -> Socket -> ( Socket, Cmd Msg )
update msg state =
    case msg of
        SocketOpened ->
            ( { state | connected = True }
            , Cmd.none
            )

        SocketClosed ->
            ( { state | connected = False }
            , Cmd.none
            )

        ProblemResponse string ->
            let
                _ =
                    state.logger "Malformed response" string
            in
            ( state, Cmd.none )

        NoOp ->
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

        Control SocketOpened ->
            Status True

        _ ->
            Control NoOp


infoDecoder : Socket -> Decoder Info
infoDecoder state =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Open" ->
                        Decode.succeed <| Control SocketOpened

                    "Data" ->
                        Decode.map Data <| Decode.field "data" Decode.value

                    _ ->
                        Decode.succeed <| Control SocketClosed
            )
