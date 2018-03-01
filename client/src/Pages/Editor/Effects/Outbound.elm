port module Pages.Editor.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        , send
        , wrapInit
        , wrapUpdate
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Debounce
import Ellie.Constants as Constants
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.User as User exposing (User)
import Elm.Package as Package exposing (Package)
import Elm.Package.Searchable as Searchable exposing (Searchable)
import Extra.Json.Encode as Encode
import Extra.Result as Result
import Extra.String as String
import Http
import Http.Extra as Http
import HttpBuilder exposing (get, post, withCredentials, withExpect, withHeader, withJsonBody, withQueryParams, withStringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Navigation
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.State as State exposing (Msg(..), State)
import Process
import Task
import WebSocket


type Outbound msg
    = GetRevision Revision.Id (Entity Revision.Id Revision -> msg)
    | GetUser (Maybe Jwt) (Jwt -> Entity User.Id User -> msg)
    | SaveSettings Jwt Settings
    | SaveToken Jwt
    | FormatElmCode String (String -> msg)
    | Compile Jwt String String (List Package)
    | ReloadIframe
    | Redirect String
    | SearchPackages String (List Searchable -> msg)
    | SwitchToDebugger
    | SwitchToProgram
    | EnableNavigationCheck Bool
    | Batch (List (Outbound msg))
    | Delay Float msg
    | None


send : (Exception -> msg) -> Outbound msg -> State msg -> ( State msg, Cmd (Msg msg) )
send onError effect state =
    case effect of
        SaveSettings token settings ->
            state.saveSettingsId
                |> Maybe.map Process.kill
                |> Maybe.withDefault (Task.succeed ())
                |> Task.andThen
                    (\_ ->
                        post "/private-api/me/settings"
                            |> withHeader "Authorization" ("Bearer " ++ Jwt.toString token)
                            |> withExpect Http.expectNoContent
                            |> withJsonBody (Settings.encoder settings)
                            |> HttpBuilder.toTask
                            |> Process.spawn
                    )
                |> Task.perform SaveSettingsSpawned
                |> (\cmd -> Debounce.push debounceSaveSettingsConfig cmd state.debounceSaveSettings)
                |> Tuple.mapFirst (\d -> { state | debounceSaveSettings = d })

        Delay time callback ->
            ( state
            , Process.sleep time
                |> Task.perform (\_ -> UserMsg callback)
            )

        EnableNavigationCheck enabled ->
            ( state
            , Encode.genericUnion "EnableNavigationCheck"
                [ Encode.bool enabled ]
                |> pagesEditorEffectsOutbound
            )

        SearchPackages query callback ->
            get "/private-api/packages/search"
                |> withQueryParams [ ( "query", query ) ]
                |> withExpect (Http.expectJson (Decode.list Searchable.decoder))
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
                |> (\cmd -> Debounce.push debouncePackageSearchConfig cmd state.debouncePackageSearch)
                |> Tuple.mapFirst (\d -> { state | debouncePackageSearch = d })

        GetRevision id callback ->
            ( state
            , get (Constants.apiBase ++ "/revisions/" ++ id.projectId ++ "/" ++ String.fromInt id.revisionNumber)
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        SaveToken token ->
            ( state
            , Encode.genericUnion "SaveToken"
                [ Encode.string (Jwt.toString token) ]
                |> pagesEditorEffectsOutbound
            )

        GetUser maybeToken callback ->
            ( state
            , maybeToken
                |> Maybe.map (\t -> "/private-api/me?token=" ++ Jwt.toString t)
                |> Maybe.withDefault "/private-api/me"
                |> post
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson getUserDecoder)
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold (uncurry callback) onError)
                |> Cmd.map UserMsg
            )

        FormatElmCode code callback ->
            ( state
            , post "/private-api/format"
                |> withHeader "Accept" "application/json"
                |> withJsonBody (Encode.object [ ( "code", Encode.string code ) ])
                |> withExpect (Http.expectJson (Decode.field "code" Decode.string))
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        Compile token elm html packages ->
            ( state
            , Encode.genericUnion "CompileRequested"
                [ Encode.string elm
                , Encode.string html
                , Encode.list <| List.map Package.encoder packages
                ]
                |> Encode.encode 0
                |> WebSocket.send (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
            )

        ReloadIframe ->
            ( state
            , Encode.genericUnion "ReloadIframe" []
                |> pagesEditorEffectsOutbound
            )

        Redirect url ->
            ( state
            , Navigation.modifyUrl url
            )

        SwitchToDebugger ->
            ( state
            , Encode.genericUnion "SwitchToDebugger" []
                |> pagesEditorEffectsOutbound
            )

        SwitchToProgram ->
            ( state
            , Encode.genericUnion "SwitchToProgram" []
                |> pagesEditorEffectsOutbound
            )

        Batch outbounds ->
            outbounds
                |> List.foldr
                    (\outbound ( state, cmds ) ->
                        let
                            ( nextState, cmd ) =
                                send onError outbound state
                        in
                        ( nextState, cmd :: cmds )
                    )
                    ( state, [] )
                |> Tuple.mapSecond Cmd.batch

        None ->
            ( state, Cmd.none )


getUserDecoder : Decoder ( Jwt, Entity User.Id User )
getUserDecoder =
    Decode.map2 (,)
        (Decode.field "token" Jwt.decoder)
        (Decode.field "user" (Entity.decoder Decode.string User.decoder))



--


port pagesEditorEffectsOutbound : Value -> Cmd msg


batch : List (Outbound msg) -> Outbound msg
batch =
    Batch


none : Outbound msg
none =
    None


wrapInit :
    (Exception -> msg)
    -> (flags -> route -> ( model, Outbound msg ))
    -> flags
    -> route
    -> ( ( model, State msg ), Cmd (Msg msg) )
wrapInit onError userInit flags route =
    let
        ( model, outbound ) =
            userInit flags route

        ( state, cmd ) =
            send onError outbound State.init
    in
    ( ( model, state ), cmd )


debouncePackageSearchConfig : Debounce.Config (Msg msg)
debouncePackageSearchConfig =
    { strategy = Debounce.later 1000
    , transform = DebouncePackageSearch
    }


debounceSaveSettingsConfig : Debounce.Config (Msg msg)
debounceSaveSettingsConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceSaveSettings
    }


wrapUpdate :
    (Exception -> msg)
    -> (msg -> model -> ( model, Outbound msg ))
    -> Msg msg
    -> ( model, State msg )
    -> ( ( model, State msg ), Cmd (Msg msg) )
wrapUpdate onError userUpdate msg (( model, state ) as appState) =
    case msg of
        UserMsg userMsg ->
            let
                ( nextModel, outbound ) =
                    userUpdate userMsg model

                ( nextState, cmd ) =
                    send onError outbound state
            in
            ( ( nextModel, nextState ), cmd )

        DebouncePackageSearch msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debouncePackageSearchConfig
                        (Debounce.takeLast identity)
                        msg
                        state.debouncePackageSearch
            in
            ( ( model, { state | debouncePackageSearch = debounce } )
            , cmd
            )

        SaveSettingsSpawned pid ->
            ( ( model, { state | saveSettingsId = Just pid } )
            , Cmd.none
            )

        DebounceSaveSettings msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceSaveSettingsConfig
                        (Debounce.takeLast identity)
                        msg
                        state.debounceSaveSettings
            in
            ( ( model, { state | debounceSaveSettings = debounce } )
            , cmd
            )

        ExceptionOccured exception ->
            let
                ( nextModel, outbound ) =
                    userUpdate (onError exception) model

                ( nextState, cmd ) =
                    send onError outbound state
            in
            ( ( nextModel, nextState ), cmd )

        NoOp ->
            ( appState, Cmd.none )


map : (a -> b) -> Outbound a -> Outbound b
map f outbound =
    case outbound of
        SaveSettings token settings ->
            SaveSettings token settings

        Delay time msg ->
            Delay time (f msg)

        EnableNavigationCheck enabled ->
            EnableNavigationCheck enabled

        GetRevision id callback ->
            GetRevision id (callback >> f)

        GetUser maybeToken callback ->
            GetUser maybeToken <| \a b -> f (callback a b)

        FormatElmCode input callback ->
            FormatElmCode input (callback >> f)

        SaveToken token ->
            SaveToken token

        SearchPackages query callback ->
            SearchPackages query (callback >> f)

        Compile token elm html packages ->
            Compile token elm html packages

        ReloadIframe ->
            ReloadIframe

        Redirect url ->
            Redirect url

        SwitchToDebugger ->
            SwitchToDebugger

        SwitchToProgram ->
            SwitchToProgram

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
