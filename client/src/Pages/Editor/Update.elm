module Pages.Editor.Update
    exposing
        ( Msg(..)
        , initialize
        , onRouteChange
        , update
        )

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.CompileStage as CompileStage exposing (CompileStage)
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Data.Ellie.Notification as Notification exposing (Notification)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Elm.Compiler.Error as CompilerError
import Data.Elm.Package as Package exposing (Package)
import Dom
import Mouse exposing (Position)
import Navigation
import Pages.Editor.Cmds as Cmds
import Pages.Editor.Model as Model exposing (EditorCollapseState(..), Flags, Model, PopoutState(..))
import Pages.Editor.Routing as Routing exposing (Route(..))
import Process
import RemoteData exposing (RemoteData(..))
import Shared.Api as Api
import Shared.Constants as Constants
import Shared.Opbeat as Opbeat
import Task
import Time exposing (Time)
import Window exposing (Size)


when : (m -> Bool) -> (m -> m) -> m -> m
when pred transform model =
    if pred model then
        transform model
    else
        model


resultIsSuccess : Result x a -> Bool
resultIsSuccess result =
    result
        |> Result.map (\_ -> True)
        |> Result.withDefault False


boolToMaybe : Bool -> Maybe ()
boolToMaybe bool =
    if bool then
        Just ()
    else
        Nothing


clamp : comparable -> comparable -> comparable -> comparable
clamp minimum maximum current =
    if current >= maximum then
        maximum
    else if current <= minimum then
        minimum
    else
        current


onlyErrors : List CompilerError.Error -> List CompilerError.Error
onlyErrors errors =
    List.filter (.level >> (==) "error") errors



-- UPDATE


type Msg
    = RouteChanged Route
    | OpenDebugger
    | LoadRevisionCompleted (Result ApiError Revision)
    | CompileRequested
    | CompileStageChanged CompileStage
    | CompileForSaveStarted Int
    | ElmCodeChanged String
    | HtmlCodeChanged String
    | SaveRequested
    | SaveCompileSucceeded (Result (List CompilerError.Error) String)
    | SaveCompileFailed String
    | SaveUploadCompleted (Result ApiError Revision)
    | OnlineChanged Bool
    | FormattingRequested
    | FormattingCompleted (Result ApiError String)
    | RemovePackageRequested Package
    | NotificationReceived Notification
    | ClearStaleNotifications Time
    | ClearAllNotifications
    | ClearNotification Notification
    | ToggleAbout
    | ResultDragStarted
    | ResultDragged Position
    | ResultDragEnded
    | EditorDragStarted
    | EditorDragged Position
    | EditorDragEnded
    | WindowSizeChanged Size
    | TitleChanged String
    | DescriptionChanged String
    | ToggleSearch
    | SearchChanged String
    | SearchResultsCompleted String (Result ApiError (List Package))
    | PackageSelected Package
    | ToggleEmbedLink
    | IframeJsError String
    | ToggleHtmlCollapse
    | ToggleElmCollapse
    | ReloadIframe
    | CreateGist
    | CreateGistComplete (Result ApiError String)
    | KeyComboMsg KeyCombo.Msg
    | NoOp


onlineNotification : Bool -> Cmd Msg
onlineNotification isOnline =
    if isOnline then
        Cmds.notify NotificationReceived
            { level = Notification.Success
            , title = "You're Online!"
            , message = "Ellie has detected that your internet connection is online."
            }
    else
        Cmds.notify NotificationReceived
            { level = Notification.Error
            , title = "You're Offline!"
            , message = "Ellie can't connect to the server right now, so we've disabled most features."
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyComboMsg keyComboMsg ->
            ( { model | keyCombo = KeyCombo.update keyComboMsg model.keyCombo }
            , Cmd.none
            )

        OpenDebugger ->
            ( model
            , Cmds.openDebugger
            )

        CreateGist ->
            let
                originalRevision =
                    model.clientRevision

                updatedRevision =
                    { originalRevision | elmCode = model.stagedElmCode, htmlCode = model.stagedHtmlCode }
            in
            ( { model | creatingGist = True }
            , Api.createGist updatedRevision
                |> Api.send CreateGistComplete
            )

        CreateGistComplete result ->
            ( { model | creatingGist = False }
            , case result of
                Ok gistUrl ->
                    Cmds.openNewWindow gistUrl

                Err apiError ->
                    Cmds.notify NotificationReceived
                        { level = Notification.Error
                        , title = "Creating Gist Failed"
                        , message = "We couldn't create a Gist for your project. Here's what GitHub said:\n" ++ apiError.explanation
                        }
            )

        ReloadIframe ->
            ( model
            , Cmds.reloadIframe
            )

        ToggleElmCollapse ->
            ( { model
                | editorsCollapse =
                    case model.editorsCollapse of
                        JustHtmlOpen ->
                            BothOpen

                        _ ->
                            JustHtmlOpen
              }
            , Cmd.none
            )

        ToggleHtmlCollapse ->
            ( { model
                | editorsCollapse =
                    case model.editorsCollapse of
                        JustElmOpen ->
                            BothOpen

                        _ ->
                            JustElmOpen
              }
            , Cmd.none
            )

        IframeJsError message ->
            ( model
            , Cmds.notify NotificationReceived
                { level = Notification.Error
                , title = "JavaScript Error"
                , message = "A JavaScript Error was thrown by your program:\n" ++ message
                }
            )

        ToggleEmbedLink ->
            ( { model
                | popoutState =
                    case model.popoutState of
                        EmbedLinkOpen ->
                            AllClosed

                        _ ->
                            EmbedLinkOpen
              }
            , Cmd.none
            )

        PackageSelected package ->
            ( { model | searchOpen = False, packagesChanged = True, searchValue = "", searchResults = [] }
                |> Model.updateClientRevision (\r -> { r | packages = r.packages ++ [ package ] })
            , Cmd.none
            )

        SearchChanged value ->
            ( { model | searchValue = value }
            , Api.searchPackages model.clientRevision.elmVersion value
                |> Api.send (SearchResultsCompleted value)
            )

        SearchResultsCompleted searchTerm result ->
            if searchTerm /= model.searchValue then
                ( model, Cmd.none )
            else
                ( { model | searchResults = Result.withDefault model.searchResults result }
                , Cmd.none
                )

        ToggleSearch ->
            if model.searchOpen then
                ( model
                    |> Model.closeSearch
                , Cmd.none
                )
            else
                ( { model | searchOpen = True }
                , Dom.focus "searchInput"
                    |> Task.attempt (\_ -> NoOp)
                )

        TitleChanged title ->
            ( model
                |> Model.updateClientRevision (\r -> { r | title = title })
            , Cmd.none
            )

        DescriptionChanged description ->
            ( model
                |> Model.updateClientRevision (\r -> { r | description = description })
            , Cmd.none
            )

        EditorDragStarted ->
            ( { model | editorDragging = True }
            , Cmd.none
            )

        EditorDragged position ->
            ( { model
                | editorSplit =
                    position
                        |> (\p -> toFloat (p.y - Constants.headerHeight))
                        |> (\h -> h / toFloat (model.windowSize.height - Constants.headerHeight))
                        |> clamp 0.2 0.8
              }
            , Cmd.none
            )

        EditorDragEnded ->
            ( { model | editorDragging = False }
            , Cmd.none
            )

        WindowSizeChanged size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        ResultDragStarted ->
            ( { model | resultDragging = True }
            , Cmd.none
            )

        ResultDragged position ->
            ( { model
                | resultSplit =
                    position
                        |> (\p -> toFloat (p.x - Constants.sidebarWidth))
                        |> (\w -> w / toFloat (model.windowSize.width - Constants.sidebarWidth))
                        |> clamp 0.2 0.8
              }
            , Cmd.none
            )

        ResultDragEnded ->
            ( { model | resultDragging = False }
            , Cmd.none
            )

        ToggleAbout ->
            ( { model
                | popoutState =
                    case model.popoutState of
                        AboutOpen ->
                            AllClosed

                        _ ->
                            AboutOpen
              }
            , Cmd.none
            )

        NotificationReceived notification ->
            ( { model
                | notifications = notification :: model.notifications
              }
            , Process.sleep (15 * Time.second)
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform ClearStaleNotifications
            )

        ClearStaleNotifications now ->
            ( { model
                | notifications =
                    List.filter
                        (\n ->
                            ((now - n.timestamp) < (15 * Time.second))
                                || (n.level == Notification.Error)
                        )
                        model.notifications
              }
            , Cmd.none
            )

        ClearAllNotifications ->
            ( { model | notifications = [] }, Cmd.none )

        ClearNotification notification ->
            ( { model
                | notifications =
                    List.filter ((/=) notification) model.notifications
              }
            , Cmd.none
            )

        LoadRevisionCompleted revisionResult ->
            ( { model
                | serverRevision = RemoteData.fromResult revisionResult
                , clientRevision = Result.withDefault model.clientRevision revisionResult
              }
                |> Model.resetStagedCode
            , case revisionResult of
                Ok _ ->
                    Cmds.notify NotificationReceived
                        { level = Notification.Success
                        , title = "Your Project Is Loaded!"
                        , message = "Ellie found the project and revision you asked for. It's loaded up and ready to be run."
                        }

                Err apiError ->
                    if apiError.statusCode == 404 then
                        Navigation.modifyUrl <| Routing.construct NewProject
                    else
                        Cmds.notify NotificationReceived
                            { level = Notification.Error
                            , title = "Failed To Load Project"
                            , message = "Ellie couldn't load the project you asked for. Here's what the server said:\n" ++ apiError.explanation
                            }
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        CompileRequested ->
            ( model
            , Cmds.compile model False
            )

        CompileStageChanged stage ->
            ( { model | compileStage = stage }
            , Cmd.none
            )

        CompileForSaveStarted totalModules ->
            ( model
            , if totalModules >= 5 then
                Cmds.notify NotificationReceived
                    { level = Notification.Info
                    , title = "Saving may take a while"
                    , message = "It looks like there are a lot of modules to compile. Please wait a moment while I build everything and save it!"
                    }
              else
                Cmd.none
            )

        ElmCodeChanged code ->
            ( model
                |> (\m -> { m | stagedElmCode = code })
            , Cmd.none
            )

        HtmlCodeChanged code ->
            ( model
                |> (\m -> { m | stagedHtmlCode = code })
            , Cmd.none
            )

        SaveRequested ->
            model
                |> (\m -> { m | saveState = Loading })
                |> Model.commitStagedCode
                |> (\m -> ( m, Cmds.compile model True ))

        SaveCompileSucceeded result ->
            ( model
            , Api.uploadRevision result model.clientRevision
                |> Task.attempt SaveUploadCompleted
            )

        SaveCompileFailed message ->
            let
                _ =
                    Debug.log "failed" message
            in
            ( model, Cmd.none )

        SaveUploadCompleted saveResult ->
            ( { model
                | saveState =
                    saveResult
                        |> Result.map (\_ -> ())
                        |> RemoteData.fromResult
                , serverRevision =
                    saveResult
                        |> Result.map Success
                        |> Result.withDefault model.serverRevision
                , clientRevision =
                    Result.withDefault model.clientRevision saveResult
              }
                |> Model.resetStagedCode
            , Cmd.batch
                [ saveResult
                    |> Result.toMaybe
                    |> Maybe.andThen .id
                    |> Maybe.map SpecificRevision
                    |> Maybe.map (Routing.construct >> Navigation.newUrl)
                    |> Maybe.withDefault Cmd.none
                , case saveResult of
                    Ok _ ->
                        Cmds.notify NotificationReceived
                            { level = Notification.Success
                            , title = "Your Project Was Saved"
                            , message = "Ellie saved your project! Your revision number has been updated in the URL."
                            }

                    Err apiError ->
                        Cmds.notify NotificationReceived
                            { level = Notification.Error
                            , title = "Failed To Save Project"
                            , message = "Ellie couldn't save your project. Here's what the server said:\n" ++ apiError.explanation
                            }
                ]
            )

        OnlineChanged isOnline ->
            ( { model | isOnline = isOnline }
            , onlineNotification isOnline
            )

        FormattingRequested ->
            ( model
            , Cmd.batch
                [ Api.format model.stagedElmCode
                    |> Api.send FormattingCompleted
                ]
            )

        FormattingCompleted result ->
            ( { model
                | stagedElmCode =
                    Result.withDefault model.stagedElmCode result
                , previousElmCode =
                    if model.previousElmCode == model.stagedElmCode then
                        Result.withDefault model.previousElmCode result
                    else
                        model.previousElmCode
              }
            , case result of
                Ok _ ->
                    Cmd.none

                Err apiError ->
                    Cmd.batch
                        [ Cmds.notify NotificationReceived
                            { level = Notification.Error
                            , title = "Formatting Your Code Failed"
                            , message = "Ellie couldn't format your code. Here's what the server said:\n" ++ apiError.explanation
                            }
                        , if apiError.statusCode == 500 then
                            Opbeat.capture
                                { tag = "FORMAT_ERROR"
                                , message = apiError.explanation
                                , moduleName = "Pages.Editor.Update"
                                , line = 498
                                , extraData = [ ( "input", model.stagedElmCode ) ]
                                }
                          else
                            Cmd.none
                        ]
            )

        RemovePackageRequested package ->
            ( { model | packagesChanged = True }
                |> Model.updateClientRevision
                    (\r -> { r | packages = List.filter ((/=) package) r.packages })
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


onRouteChange : Navigation.Location -> Msg
onRouteChange =
    Routing.parse >> RouteChanged


initialize : Flags -> Navigation.Location -> ( Model, Cmd Msg )
initialize flags location =
    let
        initialModel =
            Model.model flags
    in
    location
        |> Routing.parse
        |> (\route -> { initialModel | currentRoute = route })
        |> (\model -> ( model, Cmd.none ))
        |> (\( model, cmd ) -> handleRouteChanged model.currentRoute ( model, cmd ))


handleRouteChanged : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRouteChanged route ( model, cmd ) =
    ( case route of
        NotFound ->
            model

        NewProject ->
            Model.resetToNew model

        SpecificRevision revisionId ->
            if model.clientRevision.id == Just revisionId then
                model
            else
                { model | serverRevision = Loading, compileStage = CompileStage.Initial }
    , Cmd.batch
        [ cmd
        , Cmds.pathChanged
        , case route of
            NotFound ->
                Navigation.modifyUrl (Routing.construct NewProject)

            NewProject ->
                Api.defaultRevision |> Api.send LoadRevisionCompleted

            SpecificRevision revisionId ->
                model.clientRevision
                    |> .id
                    |> Maybe.map ((/=) revisionId)
                    |> Maybe.withDefault True
                    |> boolToMaybe
                    |> Maybe.map
                        (\() ->
                            Api.exactRevision revisionId
                                |> Api.send LoadRevisionCompleted
                        )
                    |> Maybe.withDefault Cmd.none
        ]
    )
