module Apps.Editor.Update
    exposing
        ( update
        , initialize
        , onRouteChange
        , Msg(..)
        )

import Task
import Dom
import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Navigation
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Notification as Notification exposing (Notification)
import Types.Package as Package exposing (Package)
import Types.ProjectId as ProjectId exposing (ProjectId)
import Apps.Editor.Model as Model exposing (Model, Flags, PopoutState(..), EditorCollapseState(..))
import Apps.Editor.Routing as Routing exposing (Route(..))
import Apps.Editor.Cmds as Cmds
import Shared.Api as Api
import Shared.Constants as Constants
import Shared.MessageBus as MessageBus


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


onlyErrors : List CompileError -> List CompileError
onlyErrors errors =
    List.filter (.level >> (==) "error") errors



-- UPDATE


type Msg
    = RouteChanged Route
    | LoadRevisionCompleted (Result ApiError Revision)
    | CompileRequested
    | CompileCompleted (Result ApiError (List CompileError))
    | ElmCodeChanged String
    | HtmlCodeChanged String
    | SaveRequested
    | SaveCompleted (Result ApiError Revision)
    | OnlineChanged Bool
    | FormattingRequested
    | FormattingCompleted (Result ApiError String)
    | RemovePackageRequested Package
    | NotificationReceived Notification
    | ToggleNotifications
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
    | NoOp


saveProject : Model -> Cmd Msg
saveProject model =
    if Model.isSavedProject model && Model.isOwnedProject model then
        model.clientRevision
            |> (\r -> { r | revisionNumber = Maybe.map ((+) 1) r.revisionNumber })
            |> Api.createRevision
            |> Api.send SaveCompleted
    else
        model.clientRevision
            |> Api.createProjectFromRevision
            |> Api.send SaveCompleted


onlineNotification : Bool -> Cmd Msg
onlineNotification isOnline =
    if isOnline then
        MessageBus.notify
            Notification.Success
            "You're Online!"
            "Ellie has detected that your internet connection is online."
    else
        MessageBus.notify
            Notification.Error
            "You're Offline!"
            "Ellie can't connect to the server right now, so we've disabled most features."


saveCmd : Model -> ( Model, Cmd Msg )
saveCmd model =
    ( model
    , Cmd.batch
        [ saveProject model
        , MessageBus.notify
            Notification.Info
            "Saving Your Project"
            "Ellie is saving your work! Any changes to code or settings since you last saved will be included."
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    MessageBus.notify
                        Notification.Error
                        "Creating Gist Failed"
                        ("We couldn't create a Gist for your project. Here's what GitHub said:\n\n" ++ apiError.explanation)
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
            , MessageBus.notify
                Notification.Error
                "JavaScript Error"
                ("A JavaScript Error was thrown by your program:\n\n" ++ message)
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
            , Api.searchPackages Constants.elmVersion value
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
                        |> (clamp 0.2 0.8)
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
                        |> (clamp 0.2 0.8)
              }
            , Cmd.none
            )

        ResultDragEnded ->
            ( { model | resultDragging = False }
            , Cmd.none
            )

        ToggleNotifications ->
            ( { model
                | popoutState =
                    if model.popoutState == NotificationsOpen then
                        AllClosed
                    else
                        NotificationsOpen
                , unseenNotificationsCount = 0
              }
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
                , unseenNotificationsCount =
                    if model.popoutState /= NotificationsOpen then
                        model.unseenNotificationsCount + 1
                    else if notification.level == Notification.Error then
                        0
                    else
                        model.unseenNotificationsCount
                , popoutState =
                    if notification.level == Notification.Error then
                        NotificationsOpen
                    else
                        model.popoutState
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
                    MessageBus.notify
                        Notification.Success
                        "Your Project Is Loaded!"
                        "Ellie found the project and revision you asked for. It's loaded up and ready to be run."

                Err apiError ->
                    if apiError.statusCode == 404 then
                        Navigation.modifyUrl <| Routing.construct NewProject
                    else
                        MessageBus.notify
                            Notification.Error
                            "Failed To Load Project"
                            ("Ellie couldn't load the project you asked for. Here's what the server said:\n\n" ++ apiError.explanation)
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        CompileRequested ->
            model
                |> when Model.canCompile (\m -> { m | compileResult = Loading })
                |> Cmds.withCmdWhen (\_ -> Model.canCompile model) (Cmds.compile CompileCompleted)

        CompileCompleted compileResult ->
            ( model
                |> (\m -> { m | compileResult = RemoteData.fromResult compileResult })
                |> (\m -> { m | packagesChanged = not (resultIsSuccess compileResult) })
                |> (\m -> { m | firstCompileComplete = model.firstCompileComplete || resultIsSuccess compileResult })
                |> (\m -> { m | previousElmCode = compileResult |> Result.map (\_ -> model.stagedElmCode) |> Result.withDefault model.previousElmCode })
                |> (\m -> { m | previousHtmlCode = compileResult |> Result.map (\_ -> model.stagedHtmlCode) |> Result.withDefault model.previousHtmlCode })
            , case Result.map onlyErrors compileResult of
                Ok [] ->
                    MessageBus.notify
                        Notification.Success
                        "Compilation Succeeded"
                        "Your code compiled without any problems. Nice!"

                Ok _ ->
                    MessageBus.notify
                        Notification.Warning
                        "Compilation Finished With Errors"
                        "Ellie found some compilation errors in your code. Fix them and try again!"

                Err apiError ->
                    MessageBus.notify
                        Notification.Error
                        "Compilation Failed"
                        ("Ellie couldn't even run the compiler. Here's what the server said:\n\n" ++ apiError.explanation)
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
                |> saveCmd

        SaveCompleted saveResult ->
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
                    |> Maybe.andThen (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                    |> Maybe.map
                        (\( projectId, revisionNumber ) ->
                            if Model.isOwnedProject model then
                                SpecificRevision
                                    (ProjectId.fromIdStringWithVersion (Model.activeProjectIdUrlEncoding model) projectId)
                                    revisionNumber
                            else
                                SpecificRevision (ProjectId.fromIdString projectId) revisionNumber
                        )
                    |> Maybe.map (Routing.construct >> Navigation.newUrl)
                    |> Maybe.withDefault Cmd.none
                , case saveResult of
                    Ok _ ->
                        MessageBus.notify
                            Notification.Success
                            "Your Project Was Saved"
                            "Ellie saved your project! Your revision number has been updated in the URL."

                    Err apiError ->
                        MessageBus.notify
                            Notification.Error
                            "Failed To Save Project"
                            ("Ellie couldn't save your project. Here's what the server said:\n\n" ++ apiError.explanation)
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
                , MessageBus.notify
                    Notification.Info
                    "Formatting Your Code"
                    "Ellie has asked the server to format your code with elm-format."
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
                    MessageBus.notify
                        Notification.Success
                        "Formatting Succeeded!"
                        "Your code looks so good!"

                Err apiError ->
                    MessageBus.notify
                        Notification.Error
                        "Formatting Your Code Failed"
                        ("Ellie couldn't format your code. Here's what the server said:\n\n" ++ apiError.explanation)
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

        SpecificRevision projectId revisionNumber ->
            if model.clientRevision.projectId == Just (ProjectId.toIdString projectId) && model.clientRevision.revisionNumber == Just revisionNumber then
                model
            else
                { model | serverRevision = Loading, compileResult = NotAsked }
    , Cmd.batch
        [ cmd
        , Cmds.pathChanged
        , case route of
            NotFound ->
                Navigation.modifyUrl (Routing.construct NewProject)

            NewProject ->
                Api.defaultRevision |> Api.send LoadRevisionCompleted

            SpecificRevision projectId revisionNumber ->
                model.clientRevision
                    |> (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                    |> Maybe.map (\( p, r ) -> p /= (ProjectId.toIdString projectId) || r /= revisionNumber)
                    |> Maybe.withDefault True
                    |> boolToMaybe
                    |> Maybe.map
                        (\() ->
                            Api.exactRevision (ProjectId.toIdString projectId) revisionNumber
                                |> Api.send LoadRevisionCompleted
                        )
                    |> Maybe.withDefault Cmd.none
        ]
    )
