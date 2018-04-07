port module Pages.Editor.Effects.State
    exposing
        ( Config
        , Msg(..)
        , State
        , init
        , subscriptions
        , update
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Ellie.Constants as Constants
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Types.User as User exposing (User)
import Elm.Compiler.Error as CompilerError
import Elm.Docs as Docs exposing (Module)
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Extra.HttpBuilder exposing (withMaybe)
import Extra.Json.Encode as Encode
import Extra.Result as Result
import Extra.String as String
import Http
import Http.Extra as Http
import HttpBuilder exposing (get, post, put, withBearerToken, withCredentials, withExpect, withHeader, withJsonBody, withQueryParams, withStringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keyboard
import Navigation
import Pages.Editor.Effects.Exception as Exception exposing (Exception(..))
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound(..))
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound(..))
import Process
import Task
import WebSocket


port pagesEditorEffectsStateIn : (Value -> msg) -> Sub msg


port pagesEditorEffectsStateOut : Value -> Cmd msg


processOutbound : (Exception -> msg) -> Outbound msg -> State model msg -> ( State model msg, Cmd (Msg msg) )
processOutbound onError effect state =
    case effect of
        GetDocs packages callback ->
            let
                ( inCache, needFetch ) =
                    List.partition
                        (\package -> Dict.member (Package.toString package) state.docsCache)
                        packages
            in
            ( state
            , needFetch
                |> List.map
                    (\package ->
                        Http.get (Package.docsUrl package) (Decode.list Docs.decoder)
                            |> Http.toTask
                            |> Task.map (\docs -> ( Package.toString package, docs ))
                    )
                |> Task.sequence
                |> Task.map (Dict.fromList >> Dict.union state.docsCache)
                |> Task.attempt (DocsReceived packages callback)
            )

        Navigate url ->
            ( state
            , Navigation.newUrl url
            )

        GetRevision id callback ->
            ( state
            , get ("/private-api/revisions/" ++ id.projectId ++ "/" ++ String.fromInt id.revisionNumber)
                |> withHeader "Accept" "application/json"
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold callback onError)
                |> Cmd.map UserMsg
            )

        CreateRevision token revision callback ->
            ( state
            , post "/private-api/revisions"
                |> Jwt.withTokenHeader token
                |> withJsonBody (Revision.encoder revision)
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError toString >> callback >> UserMsg)
            )

        UpdateRevision token entity callback ->
            let
                key =
                    Entity.key entity
            in
            ( state
            , put ("/private-api/revisions/" ++ key.projectId)
                |> Jwt.withTokenHeader token
                |> withJsonBody (Revision.encoder (Entity.record entity))
                |> withExpect (Http.expectJson (Entity.decoder Revision.idDecoder Revision.decoder))
                |> HttpBuilder.send (Result.mapError toString >> callback >> UserMsg)
            )

        MoveElmCursor location ->
            ( state
            , Encode.genericUnion "MoveElmCursor"
                [ Encode.int location.line
                , Encode.int location.column
                ]
                |> pagesEditorEffectsStateOut
            )

        DownloadZip { project, elm, html } ->
            ( state
            , Encode.genericUnion "DownloadZip"
                [ Encode.string <| Encode.encode 2 (Project.encoder project)
                , Encode.string elm
                , Encode.string html
                ]
                |> pagesEditorEffectsStateOut
            )

        OpenInNewTab url ->
            ( state
            , Encode.genericUnion "OpenInNewTab"
                [ Encode.string url ]
                |> pagesEditorEffectsStateOut
            )

        CreateGist stuff callback ->
            let
                body =
                    Encode.object
                        [ ( "description", Encode.string stuff.title )
                        , ( "public", Encode.bool False )
                        , ( "files"
                          , Encode.object
                                [ ( "elm.json"
                                  , Encode.object [ ( "content", Encode.string <| Encode.encode 2 (Project.encoder stuff.project) ) ]
                                  )
                                , ( "Main.elm"
                                  , Encode.object [ ( "content", Encode.string stuff.elm ) ]
                                  )
                                , ( "index.html"
                                  , Encode.object [ ( "content", Encode.string stuff.html ) ]
                                  )
                                ]
                          )
                        ]
            in
            ( state
            , post "https://api.github.com/gists"
                |> withJsonBody body
                |> withExpect (Http.expectJson (Decode.field "html_url" Decode.string))
                |> HttpBuilder.send
                    (\result ->
                        case result of
                            Ok url ->
                                UserMsg <| callback (Just url)

                            Err error ->
                                Multiple
                                    [ UserMsg <| onError <| Exception.fromHttp error
                                    , UserMsg <| callback Nothing
                                    ]
                    )
            )

        SaveSettings token settings ->
            state.saveSettingsId
                |> Maybe.map Process.kill
                |> Maybe.withDefault (Task.succeed ())
                |> Task.andThen
                    (\_ ->
                        post "/private-api/me/settings"
                            |> Jwt.withTokenHeader token
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
                |> pagesEditorEffectsStateOut
            )

        SearchPackages query callback ->
            get "/private-api/packages/search"
                |> withQueryParams [ ( "query", query ) ]
                |> withExpect (Http.expectJson (Decode.list Package.decoder))
                |> HttpBuilder.send
                    (\result ->
                        case result of
                            Ok packages ->
                                UserMsg <| callback (Just packages)

                            Err error ->
                                Multiple
                                    [ UserMsg <| onError <| Exception.fromHttp error
                                    , UserMsg <| callback Nothing
                                    ]
                    )
                |> (\cmd -> Debounce.push debouncePackageSearchConfig cmd state.debouncePackageSearch)
                |> Tuple.mapFirst (\d -> { state | debouncePackageSearch = d })

        SaveToken token ->
            ( state
            , Encode.genericUnion "SaveToken"
                [ Jwt.encoder token ]
                |> pagesEditorEffectsStateOut
            )

        Authenticate maybeToken callback ->
            ( state
            , post "/private-api/me/verify"
                |> withHeader "Accept" "application/json"
                |> withMaybe Jwt.withTokenHeader maybeToken
                |> withExpect (Http.expectJson getUserDecoder)
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold (\( a, b, c ) -> callback a b c) onError)
                |> Cmd.map UserMsg
            )

        AcceptTerms token termsVersion msg ->
            ( state
            , post "/private-api/me/terms"
                |> Jwt.withTokenHeader token
                |> withJsonBody (Encode.object [ ( "termsVersion", TermsVersion.encoder termsVersion ) ])
                |> withExpect Http.expectNoContent
                |> HttpBuilder.send (Result.mapError Exception.fromHttp >> Result.fold (\_ -> msg) onError)
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
                |> pagesEditorEffectsStateOut
            )

        Redirect url ->
            ( state
            , Navigation.modifyUrl url
            )

        SwitchToDebugger ->
            ( state
            , Encode.genericUnion "SwitchToDebugger" []
                |> pagesEditorEffectsStateOut
            )

        SwitchToProgram ->
            ( state
            , Encode.genericUnion "SwitchToProgram" []
                |> pagesEditorEffectsStateOut
            )

        Outbound.Batch outbounds ->
            outbounds
                |> List.foldr
                    (\outbound ( state, cmds ) ->
                        let
                            ( nextState, cmd ) =
                                processOutbound onError outbound state
                        in
                        ( nextState, cmd :: cmds )
                    )
                    ( state, [] )
                |> Tuple.mapSecond Cmd.batch

        Outbound.None ->
            ( state, Cmd.none )


getUserDecoder : Decoder ( TermsVersion, Jwt, Entity User.Id User )
getUserDecoder =
    Decode.map3 (,,)
        (Decode.field "latestTerms" TermsVersion.decoder)
        (Decode.field "token" Jwt.decoder)
        (Decode.field "user" (Entity.decoder User.idDecoder User.decoder))


processInbound : Inbound msg -> Sub (Msg msg)
processInbound inbound =
    case inbound of
        WorkspaceDetached token next ->
            Decode.string
                |> Decode.map
                    (\url ->
                        if url == (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token) then
                            UserMsg next
                        else
                            NoOp
                    )
                |> portDecoder "SocketClosed"
                |> pagesEditorEffectsStateIn

        EscapePressed next ->
            Keyboard.ups
                (\keycode ->
                    if keycode == 27 then
                        UserMsg next
                    else
                        NoOp
                )

        KeepWorkspaceOpen token ->
            WebSocket.keepAlive (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        CompileFinished token callback ->
            Decode.list CompilerError.decoder
                |> Decode.map (callback >> UserMsg)
                |> socketDecoder "CompileFinished"
                |> WebSocket.listen (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        WorkspaceAttached token callback ->
            Decode.list Package.decoder
                |> Decode.map (callback >> UserMsg)
                |> socketDecoder "WorkspaceAttached"
                |> WebSocket.listen (Constants.workspaceUrl ++ "?token=" ++ Jwt.toString token)

        OutputThrewException callback ->
            Decode.string
                |> Decode.map (callback >> UserMsg)
                |> portDecoder "OutputThrewException"
                |> pagesEditorEffectsStateIn

        Inbound.Batch inbounds ->
            Sub.batch <| List.map processInbound inbounds

        Inbound.None ->
            Sub.none


socketDecoder : String -> Decoder (Msg msg) -> String -> Msg msg
socketDecoder topic decoder message =
    let
        messageDecoder =
            Decode.field "topic" Decode.string
                |> Decode.andThen
                    (\realTopic ->
                        if realTopic == topic then
                            Decode.oneOf
                                [ Decode.field "contents" decoder
                                , Decode.map ExceptionOccured <| Decode.field "exception" Exception.decoder
                                ]
                        else
                            Decode.succeed NoOp
                    )
    in
    message
        |> Decode.decodeString messageDecoder
        |> Result.fold identity (ClientDecoderFailure >> ExceptionOccured)


portDecoder : String -> Decoder (Msg msg) -> Value -> Msg msg
portDecoder topic decoder message =
    let
        decoder =
            Decode.field "topic" Decode.string
                |> Decode.andThen
                    (\realTopic ->
                        if realTopic == topic then
                            Decode.oneOf
                                [ Decode.map ExceptionOccured <| Decode.field "exception" Exception.decoder
                                , Decode.field "contents" decoder
                                ]
                        else
                            Decode.succeed NoOp
                    )
    in
    message
        |> Decode.decodeValue decoder
        |> Result.fold identity (ClientDecoderFailure >> ExceptionOccured)



----


type Msg msg
    = UserMsg msg
    | DebouncePackageSearch Debounce.Msg
    | SaveSettingsSpawned Process.Id
    | DebounceSaveSettings Debounce.Msg
    | DocsReceived (List Package) (List Module -> msg) (Result Http.Error (Dict String (List Module)))
    | ExceptionOccured Exception
    | Multiple (List (Msg msg))
    | NoOp


type alias State model msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    , getUserTagger : Maybe (User -> msg)
    , saveSettingsId : Maybe Process.Id
    , debounceSaveSettings : Debounce (Cmd (Msg msg))
    , docsCache : Dict String (List Module)
    , model : model
    }


type alias Config msg flags route model =
    { onError : Exception -> msg
    , userUpdate : msg -> model -> ( model, Outbound msg )
    , userInit : flags -> route -> ( model, Outbound msg )
    , userSubs : model -> Inbound msg
    }


subscriptions : Config msg flags route model -> State model msg -> Sub (Msg msg)
subscriptions config state =
    processInbound <| config.userSubs state.model


init :
    Config msg flags route model
    -> flags
    -> route
    -> ( State model msg, Cmd (Msg msg) )
init { onError, userInit } flags route =
    let
        ( model, outbound ) =
            userInit flags route

        initialState =
            { debouncePackageSearch = Debounce.init
            , getUserTagger = Nothing
            , saveSettingsId = Nothing
            , debounceSaveSettings = Debounce.init
            , docsCache = Dict.empty
            , model = model
            }

        ( state, cmd ) =
            processOutbound onError outbound initialState
    in
    ( state
    , cmd
    )


debouncePackageSearchConfig : Debounce.Config (Msg msg)
debouncePackageSearchConfig =
    { strategy = Debounce.later 500
    , transform = DebouncePackageSearch
    }


debounceSaveSettingsConfig : Debounce.Config (Msg msg)
debounceSaveSettingsConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceSaveSettings
    }


update :
    Config msg flags route model
    -> Msg msg
    -> State model msg
    -> ( State model msg, Cmd (Msg msg) )
update ({ onError, userUpdate } as config) msg state =
    case msg of
        DocsReceived packages callback cacheResult ->
            case cacheResult of
                Ok cache ->
                    packages
                        |> List.filterMap (\p -> Dict.get (Package.toString p) cache)
                        |> List.concat
                        |> callback
                        |> UserMsg
                        |> (\m -> update config m { state | docsCache = cache })

                Err _ ->
                    ( state, Cmd.none )

        Multiple msgs ->
            List.foldl
                (\nextMsg ( model, cmds ) ->
                    let
                        ( nextModel, nextCmd ) =
                            update config nextMsg model
                    in
                    ( nextModel, nextCmd :: cmds )
                )
                ( state, [] )
                msgs
                |> Tuple.mapSecond Cmd.batch

        UserMsg userMsg ->
            let
                ( nextModel, outbound ) =
                    userUpdate userMsg state.model

                ( nextState, cmd ) =
                    processOutbound onError outbound state
            in
            ( { nextState | model = nextModel }, cmd )

        DebouncePackageSearch msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debouncePackageSearchConfig
                        (Debounce.takeLast identity)
                        msg
                        state.debouncePackageSearch
            in
            ( { state | debouncePackageSearch = debounce }
            , cmd
            )

        SaveSettingsSpawned pid ->
            ( { state | saveSettingsId = Just pid }
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
            ( { state | debounceSaveSettings = debounce }
            , cmd
            )

        ExceptionOccured exception ->
            let
                ( nextModel, outbound ) =
                    userUpdate (onError exception) state.model

                ( nextState, cmd ) =
                    processOutbound onError outbound { state | model = nextModel }
            in
            ( nextState, cmd )

        NoOp ->
            ( state, Cmd.none )
