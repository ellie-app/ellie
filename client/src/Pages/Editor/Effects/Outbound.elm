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
import Elm.Package.Searchable as Searchable exposing (Searchable)
import Extra.Result as Result
import Extra.String as String
import Http
import Http.Extra as Http
import HttpBuilder exposing (get, post, withCredentials, withExpect, withHeader, withJsonBody, withQueryParams, withStringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Navigation
import Pages.Editor.Effects.Error as Error exposing (Error)
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
    | CompileElmCode Jwt String
    | ReloadIframe Bool
    | Redirect String
    | AttachToWorkspace Jwt
    | SearchPackages String (List Searchable -> msg)
    | SwitchToDebugger
    | SwitchToProgram
    | EnableNavigationCheck Bool
    | Batch (List (Outbound msg))
    | Delay Float msg
    | None


send : (Error -> msg) -> Outbound msg -> State msg -> ( State msg, Cmd (Msg msg) )
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
            , genericUnion "EnableNavigationCheck" [ Encode.bool enabled ]
                |> pagesEditorEffectsOutbound
            )

        SearchPackages query callback ->
            get "/private-api/packages/search"
                |> withQueryParams [ ( "query", query ) ]
                |> withExpect (Http.expectJson (Decode.list Searchable.decoder))
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
                |> (\cmd -> Debounce.push debouncePackageSearchConfig cmd state.debouncePackageSearch)
                |> Tuple.mapFirst (\d -> { state | debouncePackageSearch = d })

        GetRevision id callback ->
            ( state
            , get (Constants.apiBase ++ "/revisions/" ++ id.projectId ++ "/" ++ String.fromInt id.revisionNumber)
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        SaveToken token ->
            ( state
            , genericUnion "SaveToken" [ Encode.string (Jwt.toString token) ]
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
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold (uncurry callback) onError)
                |> Cmd.map UserMsg
            )

        FormatElmCode code callback ->
            ( state
            , post (Constants.apiBase ++ "/format")
                |> withHeader "Accept" "application/elm"
                |> withExpect Http.expectString
                |> withStringBody "application/elm" code
                |> HttpBuilder.send (Result.mapError Error.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        CompileElmCode token code ->
            ( state
            , Encode.object
                [ ( "tag", Encode.string "CompileRequested" )
                , ( "contents", Encode.list [ Encode.string code ] )
                ]
                |> Encode.encode 0
                |> WebSocket.send (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
            )

        ReloadIframe debugger ->
            ( state
            , [ Encode.bool debugger ]
                |> genericUnion "ReloadIframe"
                |> pagesEditorEffectsOutbound
            )

        Redirect url ->
            ( state
            , Navigation.modifyUrl url
            )

        AttachToWorkspace token ->
            ( state
            , genericUnion "AttachToWorkspace" []
                |> Encode.encode 0
                |> WebSocket.send (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)
            )

        SwitchToDebugger ->
            ( state
            , genericUnion "SwitchToDebugger" []
                |> pagesEditorEffectsOutbound
            )

        SwitchToProgram ->
            ( state
            , genericUnion "SwitchToProgram" []
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


port pagesEditorEffectsOutbound : Value -> Cmd msg


batch : List (Outbound msg) -> Outbound msg
batch =
    Batch


none : Outbound msg
none =
    None


wrapInit :
    (Error -> msg)
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
    (Error -> msg)
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

        CompileElmCode token input ->
            CompileElmCode token input

        ReloadIframe debugger ->
            ReloadIframe debugger

        Redirect url ->
            Redirect url

        AttachToWorkspace token ->
            AttachToWorkspace token

        SwitchToDebugger ->
            SwitchToDebugger

        SwitchToProgram ->
            SwitchToProgram

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
