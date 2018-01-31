port module Ellie.Effect.Outbound
    exposing
        ( Outbound(..)
        , send
        , wrap
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Constants as Constants
import Ellie.Effect.Error as Error exposing (Error)
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.User as User exposing (User)
import Extra.Result as Result
import Extra.String as String
import Http
import HttpBuilder exposing (get, post, withCredentials, withExpect, withHeader, withStringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Navigation
import WebSocket


port ellieEffectsOutbound : Value -> Cmd msg


type Outbound msg
    = GetRevision Revision.Id (Entity Revision.Id Revision -> msg)
    | GetUser (Maybe Jwt) (Jwt -> Entity User.Id User -> msg)
    | SaveToken Jwt
    | FormatElmCode String (String -> msg)
    | CompileElmCode Jwt String
    | ReloadIframe String
    | Redirect String
    | AttachToWorkspace Jwt


send : (Error -> msg) -> Outbound msg -> Cmd msg
send onError effect =
    case effect of
        GetRevision id callback ->
            get (Constants.apiBase ++ "/revisions/" ++ id.projectId ++ "/" ++ String.fromInt id.revisionNumber)
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold callback onError)

        SaveToken token ->
            Encode.object
                [ ( "tag", Encode.string "SaveToken" )
                , ( "contents", Encode.list [ Encode.string (Jwt.toString token) ] )
                ]
                |> ellieEffectsOutbound

        GetUser maybeToken callback ->
            maybeToken
                |> Maybe.map (\t -> "/private-api/me?token=" ++ Jwt.toString t)
                |> Maybe.withDefault "/private-api/me"
                |> post
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson getUserDecoder)
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold (uncurry callback) onError)

        FormatElmCode code callback ->
            post (Constants.apiBase ++ "/format")
                |> withHeader "Accept" "application/elm"
                |> withExpect Http.expectString
                |> withStringBody "application/elm" code
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold callback onError)

        CompileElmCode token code ->
            Encode.object
                [ ( "tag", Encode.string "CompileRequested" )
                , ( "contents", Encode.list [ Encode.string code ] )
                ]
                |> Encode.encode 0
                |> WebSocket.send (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        ReloadIframe iframeId ->
            ellieEffectsOutbound <|
                genericUnion
                    "ReloadIframe"
                    [ Encode.string iframeId ]

        Redirect url ->
            Navigation.modifyUrl url

        AttachToWorkspace token ->
            genericUnion "AttachToWorkspace" []
                |> Encode.encode 0
                |> WebSocket.send (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)


genericUnion : String -> List Value -> Value
genericUnion tag contents =
    Encode.object
        [ ( "tag", Encode.string tag )
        , ( "contents", Encode.list contents )
        ]


getUserDecoder : Decoder ( Jwt, Entity User.Id User )
getUserDecoder =
    Decode.map2 (,)
        (Decode.field "token" Jwt.decoder)
        (Decode.field "user" (Entity.decoder Decode.string User.decoder))



--


wrap : (Error -> msg) -> ( model, List (Outbound msg) ) -> ( model, Cmd msg )
wrap onError ( model, outbounds ) =
    ( model, Cmd.batch (List.map (send onError) outbounds) )
