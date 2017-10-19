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
import Ellie.Api as Api
import Ellie.CodeMirror as CodeMirror
import Ellie.Opbeat as Opbeat
import Navigation
import Pages.Editor.Cmds as Cmds
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Header.Update as Header
import Pages.Editor.Layout.Update as Layout
import Pages.Editor.Logs.Update as Logs
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Routing as Routing exposing (Route(..))
import Pages.Editor.Save.Update as Save
import Pages.Editor.Sidebar.Model as Sidebar
import Pages.Editor.Sidebar.Update as Sidebar
import Process
import RemoteData exposing (RemoteData(..))
import Task
import Time exposing (Time)


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
    | LoadRevisionCompleted (Result ApiError Revision)
    | CompileRequested
    | CompileStageChanged CompileStage
    | CompileForSaveStarted Int
    | OnlineChanged Bool
    | FormattingRequested
    | FormattingCompleted (Result ApiError String)
    | NotificationReceived Notification
    | ClearStaleNotifications Time
    | ClearAllNotifications
    | ClearNotification Notification
    | TitleChanged String
    | DescriptionChanged String
    | RemovePackageRequested Package
    | PackageSelected Package
    | IframeJsError String
    | CreateGist
    | CreateGistComplete (Result ApiError String)
    | ClearElmStuff
    | ToggleVimMode Bool
    | NoOp
      -- CodeMirror Stuff
    | ElmCodeChanged String
    | HtmlCodeChanged String
      -- Error Handling
    | ReportException Opbeat.Exception
      -- Nested Stuff
    | SaveMsg Save.Msg
    | KeyComboMsg KeyCombo.Msg
    | HeaderMsg Header.Msg
    | SidebarMsg Sidebar.Msg
    | LayoutMsg Layout.Msg
    | LogsMsg Logs.Msg


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
        LogsMsg logsMsg ->
            ( { model | logs = Logs.update logsMsg model.logs }
            , Cmd.none
            )

        ToggleVimMode enabled ->
            ( { model | vimMode = enabled }
            , Cmd.batch
                [ CodeMirror.updateVimMode "elmEditor" enabled
                , CodeMirror.updateVimMode "htmlEditor" enabled
                , Cmds.saveVimMode enabled
                ]
            )

        ReportException exception ->
            ( model
            , Opbeat.capture exception
            )

        SidebarMsg sidebarMsg ->
            model.sidebar
                |> Sidebar.update model.clientRevision.elmVersion sidebarMsg
                |> Tuple.mapFirst (\s -> { model | sidebar = s })
                |> Tuple.mapSecond (Cmd.map SidebarMsg)

        LayoutMsg layoutMsg ->
            ( { model | layout = Layout.update layoutMsg model.layout }
            , Cmd.none
            )

        HeaderMsg headerMsg ->
            ( { model | header = Header.update headerMsg model.header }
            , Cmd.none
            )

        ClearElmStuff ->
            ( model
            , Cmd.batch
                [ Cmds.clearElmStuff
                , Cmds.notify NotificationReceived
                    { level = Notification.Info
                    , title = "Compiler Cache Cleared"
                    , message = "All cached compiler artifacts have been removed. Your next build may take a while!"
                    }
                ]
            )

        KeyComboMsg keyComboMsg ->
            ( { model | keyCombo = KeyCombo.update keyComboMsg model.keyCombo }
            , Cmd.none
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

        IframeJsError message ->
            ( model
            , Cmds.notify NotificationReceived
                { level = Notification.Error
                , title = "JavaScript Error"
                , message = "A JavaScript Error was thrown by your program:\n" ++ message
                }
            )

        PackageSelected package ->
            ( { model
                | packagesChanged = True
                , sidebar = Sidebar.resetSearch model.sidebar
              }
                |> Model.updateClientRevision (\r -> { r | packages = r.packages ++ [ package ] })
            , Cmd.none
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

        NotificationReceived notification ->
            ( { model
                | notifications =
                    notification
                        :: List.filter (Notification.eq notification >> not) model.notifications
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
                Ok result ->
                    Cmd.batch
                        [ Cmds.notify NotificationReceived
                            { level = Notification.Success
                            , title = "Your Project Is Loaded!"
                            , message = "Ellie found the project and revision you asked for. It's loaded up and ready to be run."
                            }
                        , CodeMirror.updateValue "elmEditor" result.elmCode
                        , CodeMirror.updateValue "htmlEditor" result.htmlCode
                        ]

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
            , case stage of
                CompileStage.FinishedWithErrors compilerErrors ->
                    CodeMirror.updateLinter "elmEditor" <|
                        List.map CompilerError.toLinterMessage compilerErrors

                _ ->
                    CodeMirror.updateLinter "elmEditor" []
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

        FormattingCompleted (Ok code) ->
            ( { model
                | stagedElmCode = code
                , previousElmCode =
                    if model.previousElmCode == model.stagedElmCode then
                        code
                    else
                        model.previousElmCode
              }
            , CodeMirror.updateValue "elmEditor" code
            )

        FormattingCompleted (Err apiError) ->
            ( model
            , Cmd.batch
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
                        , line = 480
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

        SaveMsg saveMsg ->
            let
                ( nextModel, notification, cmd ) =
                    Save.update model saveMsg
            in
            ( nextModel
            , Cmd.batch
                [ notification
                    |> Maybe.map (Cmds.notify NotificationReceived)
                    |> Maybe.withDefault Cmd.none
                , Cmd.map SaveMsg cmd
                ]
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

        model =
            { initialModel | currentRoute = Routing.parse location }
    in
    handleRouteChanged
        model.currentRoute
        ( model
        , Cmd.batch
            [ CodeMirror.setup "elmEditor"
                { vimMode = model.vimMode
                , theme = "material"
                , mode = "elm"
                , initialValue = model.clientRevision.elmCode
                , readOnly = False
                , tabSize = 4
                }
            , CodeMirror.setup "htmlEditor"
                { vimMode = model.vimMode
                , theme = "material"
                , mode = "htmlmixed"
                , initialValue = model.clientRevision.htmlCode
                , readOnly = False
                , tabSize = 2
                }
            ]
        )


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
