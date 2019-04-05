port module Network.Absinthe.Socket exposing (Info(..), Socket, init, listen)

import Extra.Json.Encode as Encode
import Graphql.Document as Document
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port absintheSocketOutbound : Value -> Cmd msg


port absintheSocketInbound : (Value -> msg) -> Sub msg


listen : Socket -> Sub Info
listen state =
    absintheSocketInbound <|
        \input ->
            case Decode.decodeValue (infoDecoder state) input of
                Ok info ->
                    info

                Err _ ->
                    Status False


type alias Socket =
    { url : String
    , token : Maybe String
    , document : String
    }


init : { url : String, token : Maybe String } -> SelectionSet a RootSubscription -> ( Socket, Cmd msg )
init { url, token } doc =
    ( { url = url
      , token = token
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


infoDecoder : Socket -> Decoder Info
infoDecoder state =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Open" ->
                        Decode.succeed <| Status True

                    "Data" ->
                        Decode.map Data <| Decode.field "data" Decode.value

                    _ ->
                        Decode.succeed <| Status False
            )
