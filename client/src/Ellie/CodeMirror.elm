port module Ellie.CodeMirror
    exposing
        ( Id
        , Inbound(..)
        , setup
        , subscriptions
        , updateLinter
        , updateValue
        , updateVimMode
        )

import Data.CodeMirror.LinterMessage as LinterMessage exposing (LinterMessage)
import Data.CodeMirror.Options as Options exposing (Options)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Shared.Opbeat as Opbeat


port ellieCodeMirrorOut : Value -> Cmd msg


port ellieCodeMirrorIn : (Value -> msg) -> Sub msg


type alias Id =
    String


type Outbound
    = Setup Id Options
    | UpdateVimMode Id Bool
    | UpdateValue Id String
    | UpdateLinter Id (List LinterMessage)


outboundEncoder : Outbound -> Value
outboundEncoder outbound =
    case outbound of
        Setup id options ->
            Encode.object
                [ ( "tag", Encode.string "Setup" )
                , ( "id", Encode.string id )
                , ( "options", Options.encoder options )
                ]

        UpdateVimMode id enabled ->
            Encode.object
                [ ( "tag", Encode.string "UpdateVimMode" )
                , ( "id", Encode.string id )
                , ( "enabled", Encode.bool enabled )
                ]

        UpdateValue id value ->
            Encode.object
                [ ( "tag", Encode.string "UpdateValue" )
                , ( "id", Encode.string id )
                , ( "value", Encode.string value )
                ]

        UpdateLinter id messages ->
            Encode.object
                [ ( "tag", Encode.string "UpdateLinter" )
                , ( "id", Encode.string id )
                , ( "messages", Encode.list <| List.map LinterMessage.encoder messages )
                ]


type Inbound
    = ValueChanged String String


inboundDecoder : Decoder Inbound
inboundDecoder =
    Decode.andThen
        (\tag ->
            case tag of
                "ValueChanged" ->
                    Decode.map2 ValueChanged
                        (Decode.field "id" Decode.string)
                        (Decode.field "value" Decode.string)

                _ ->
                    Decode.fail <| "Unexpected port message from Views/Editors/Runner.js: \"" ++ tag ++ "\""
        )
        (Decode.field "tag" Decode.string)


subscriptions : Sub (Result Opbeat.Exception Inbound)
subscriptions =
    ellieCodeMirrorIn <|
        \value ->
            case Decode.decodeValue inboundDecoder value of
                Ok data ->
                    Ok data

                Err message ->
                    Err <|
                        { tag = "UnknownInboundPortMessage"
                        , message = message
                        , moduleName = "Views.Editors"
                        , line = 94
                        , extraData = [ ( "original", Encode.encode 2 value ) ]
                        }


setup : Id -> Options -> Cmd msg
setup id options =
    Setup id options
        |> outboundEncoder
        |> ellieCodeMirrorOut


updateValue : Id -> String -> Cmd msg
updateValue id value =
    UpdateValue id value
        |> outboundEncoder
        |> ellieCodeMirrorOut


updateLinter : Id -> List LinterMessage -> Cmd msg
updateLinter id messages =
    UpdateLinter id messages
        |> outboundEncoder
        |> ellieCodeMirrorOut


updateVimMode : Id -> Bool -> Cmd msg
updateVimMode id enabled =
    UpdateVimMode id enabled
        |> outboundEncoder
        |> ellieCodeMirrorOut
