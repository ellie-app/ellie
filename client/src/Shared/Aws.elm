port module Shared.Aws
    exposing
        ( Content(..)
        , Id
        , Inbound(..)
        , MimeType
        , UploadInfo
        , Url
        , subscriptions
        , upload
        , uploadBatch
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Shared.Opbeat as Opbeat


port sharedAwsOut : Value -> Cmd msg


port sharedAwsIn : (Value -> msg) -> Sub msg


type alias Id =
    String


type alias Url =
    String


type alias MimeType =
    String


type Content
    = Stream Url
    | Direct String


type alias UploadInfo =
    { name : String
    , url : Url
    , mime : MimeType
    , content : Content
    , fields : List ( String, String )
    }


type Outbound
    = UploadStart Id UploadInfo
    | UploadBatch Id (List UploadInfo)


type Inbound
    = UploadSucceeded { id : Id }
    | UploadFailed { id : Id, message : String }


upload : Id -> UploadInfo -> Cmd msg
upload id info =
    UploadStart id info
        |> outboundEncoder
        |> sharedAwsOut


uploadBatch : Id -> List UploadInfo -> Cmd msg
uploadBatch id infos =
    UploadBatch id infos
        |> outboundEncoder
        |> sharedAwsOut


subscriptions : Sub (Result Opbeat.Exception Inbound)
subscriptions =
    sharedAwsIn <|
        \value ->
            case Decode.decodeValue inboundDecoder value of
                Ok data ->
                    Ok data

                Err message ->
                    Err <|
                        { tag = "UnknownInboundPortMessage"
                        , message = message
                        , moduleName = "Shared.Aws"
                        , line = 94
                        , extraData = [ ( "original", Encode.encode 2 value ) ]
                        }


contentEncoder : Content -> Value
contentEncoder content =
    case content of
        Stream url ->
            Encode.object
                [ ( "tag", Encode.string "Stream" )
                , ( "url", Encode.string url )
                ]

        Direct data ->
            Encode.object
                [ ( "tag", Encode.string "Direct" )
                , ( "data", Encode.string data )
                ]


uploadInfoEncoder : UploadInfo -> Value
uploadInfoEncoder { name, mime, url, content, fields } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "mime", Encode.string mime )
        , ( "url", Encode.string url )
        , ( "content", contentEncoder content )
        , ( "fields"
          , fields
                |> List.map (\( l, r ) -> Encode.list [ Encode.string l, Encode.string r ])
                |> Encode.list
          )
        ]


outboundEncoder : Outbound -> Value
outboundEncoder outbound =
    case outbound of
        UploadStart id info ->
            Encode.object
                [ ( "tag", Encode.string "UploadStart" )
                , ( "id", Encode.string id )
                , ( "info", uploadInfoEncoder info )
                ]

        UploadBatch id infos ->
            Encode.object
                [ ( "tag", Encode.string "UploadBatch" )
                , ( "id", Encode.string id )
                , ( "infos", Encode.list <| List.map uploadInfoEncoder infos )
                ]


inboundDecoder : Decoder Inbound
inboundDecoder =
    Decode.andThen
        (\tag ->
            case tag of
                "UploadSucceeded" ->
                    Decode.field "id" Decode.string
                        |> Decode.map (\id -> UploadSucceeded { id = id })

                "UploadFailed" ->
                    Decode.map2 (\id message -> UploadFailed { id = id, message = message })
                        (Decode.field "id" Decode.string)
                        (Decode.field "message" Decode.string)

                _ ->
                    Decode.fail <| "Unexpected port message from Shared/Aws/Runner.js: \"" ++ tag ++ "\""
        )
        (Decode.field "tag" Decode.string)
