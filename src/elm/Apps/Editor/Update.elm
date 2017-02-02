module Apps.Editor.Update
    exposing
        ( update
        , initialize
        , onRouteChange
        , Msg(..)
        )

import Task
import Dom
import Set exposing (Set)
import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Navigation
import Types.Dependency as Dependency exposing (Dependency)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.Session as Session exposing (Session)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Notification as Notification exposing (Notification)
import Types.Package as Package exposing (Package)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.Version as Version exposing (Version)
import Apps.Editor.Model as Model exposing (Model, Flags, PopoutState(..))
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
    = CreateSessionCompleted (Result ApiError Session)
    | RouteChanged Route
    | LoadRevisionCompleted (Result ApiError Revision)
    | CompileRequested
    | CompileCompleted (Result ApiError (List CompileError))
    | WriteIframeCompleted (Result ApiError ())
    | ElmCodeChanged String
    | HtmlCodeChanged String
    | SaveRequested
    | SaveCompleted (Result ApiError Revision)
    | OnlineChanged Bool
    | FormattingRequested
    | FormattingCompleted (Result ApiError String)
    | RemoveDependencyRequested Dependency
    | RemoveDependencyCompleted (Result ApiError Dependency)
    | WindowUnloaded
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
    | AddDependencyCompleted Dependency (Result ApiError ())
    | ToggleEmbedLink
    | NoOp


saveProject : Model -> Cmd Msg
saveProject model =
    case model.session of
        Success session ->
            if Model.isSavedProject model && Model.isOwnedProject model then
                model.clientRevision
                    |> (\r -> { r | revisionNumber = Maybe.map ((+) 1) r.revisionNumber })
                    |> Api.createRevision session
                    |> Api.send SaveCompleted
            else
                model.clientRevision
                    |> Api.createProjectFromRevision session
                    |> Api.send SaveCompleted

        _ ->
            Cmd.none


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


addDependencyCmd : Package -> Model -> ( Model, Cmd Msg )
addDependencyCmd package model =
    let
        dep =
            Dependency
                package.username
                package.name
                (VersionRange package.version (Version.nextMajor package.version))
    in
        model.session
            |> RemoteData.map (\session -> Api.addDependencies session [ dep ])
            |> RemoteData.map
                (\req ->
                    Cmd.batch
                        [ Api.send (AddDependencyCompleted dep) req
                        , MessageBus.notify
                            Notification.Info
                            "Installing Package"
                            "Ellie is installing your package. Once it's installed and compiled you'll be able to use it in your code."
                        ]
                )
            |> RemoteData.map ((,) model)
            |> RemoteData.withDefault ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        AddDependencyCompleted dependency result ->
            ( { model
                | installingPackage =
                    result
                        |> Result.map (\_ -> Nothing)
                        |> Result.withDefault model.installingPackage
              }
                |> Model.updateClientRevision
                    (\r ->
                        { r
                            | dependencies =
                                r.dependencies ++ (result |> Result.map (\_ -> [ dependency ]) |> Result.withDefault [])
                        }
                    )
            , case result of
                Ok _ ->
                    MessageBus.notify
                        Notification.Success
                        "Package Installed!"
                        "Ellie installed and compiled your package. You can start using it in your code now."

                Err apiError ->
                    MessageBus.notify
                        Notification.Error
                        "Failed Installing Package"
                        ("Elle couldn't install your package. Here's what the server said:\n\n" ++ apiError.explanation)
            )

        PackageSelected package ->
            { model | installingPackage = Just package }
                |> Model.closeSearch
                |> addDependencyCmd package

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

        WindowUnloaded ->
            ( model
            , model.session
                |> RemoteData.map Api.removeSession
                |> RemoteData.map (Api.send (\_ -> NoOp))
                |> RemoteData.withDefault Cmd.none
            )

        CreateSessionCompleted sessionResult ->
            ( { model | session = RemoteData.fromResult sessionResult }
            , Cmd.batch
                [ onlineNotification model.isOnline
                , case sessionResult of
                    Ok _ ->
                        MessageBus.notify
                            Notification.Success
                            "Your Session is Ready!"
                            "Your session is all set! Compile away."

                    Err apiError ->
                        MessageBus.notify
                            Notification.Error
                            "Failed To Set Up Session"
                            ("Ellie couldn't set up a session for you right now. Here's what the server said:\n\n" ++ apiError.explanation)
                ]
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
                    MessageBus.notify
                        Notification.Error
                        "Failed To Load Project"
                        ("Ellie couldn't load the project you asked for. Here's what the server said:\n\n" ++ apiError.explanation)
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        CompileRequested ->
            model
                |> when Model.shouldCompileElm (\m -> { m | compileResult = Loading })
                |> when Model.shouldWriteIframe (\m -> { m | iframeResult = Loading })
                |> Cmds.withCmd (Cmds.compile CompileCompleted)
                |> Cmds.withAdditionalCmd (Cmds.writeIframe (RemoteData.isNotAsked model.iframeResult) WriteIframeCompleted)

        WriteIframeCompleted result ->
            model
                |> (\m -> { m | iframeResult = RemoteData.fromResult result })
                |> (\m -> { m | previousHtmlCode = result |> Result.map (always m.stagedHtmlCode) |> Result.withDefault m.previousHtmlCode })
                |> Cmds.withCmd (Cmds.notifyIframeResult result)

        CompileCompleted compileResult ->
            ( model
                |> (\m -> { m | compileResult = RemoteData.fromResult compileResult })
                |> (\m -> { m | firstCompileComplete = model.firstCompileComplete || resultIsSuccess compileResult })
                |> (\m -> { m | previousElmCode = compileResult |> Result.map (\_ -> model.stagedElmCode) |> Result.withDefault model.previousElmCode })
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
                    |> Maybe.map (\( p, r ) -> Routing.construct <| SpecificRevision p r)
                    |> Maybe.map Navigation.newUrl
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

        RemoveDependencyRequested dependency ->
            ( { model
                | removingDependencyHashes =
                    Set.insert (Dependency.hash dependency) model.removingDependencyHashes
              }
            , Cmd.batch
                [ model.session
                    |> RemoteData.map (\s -> Api.removeDependency s dependency)
                    |> RemoteData.map (Api.send RemoveDependencyCompleted)
                    |> RemoteData.withDefault Cmd.none
                , MessageBus.notify
                    Notification.Info
                    "Removing Package"
                    "Ellie is removing your package. Once it's gone you'll have to reinstall it to use it again."
                ]
            )

        RemoveDependencyCompleted result ->
            ( case result of
                Ok dep ->
                    { model | removingDependencyHashes = Set.remove (Dependency.hash dep) model.removingDependencyHashes }
                        |> Model.updateClientRevision (\r -> { r | dependencies = List.filter ((/=) dep) r.dependencies })

                Err _ ->
                    model
            , case result of
                Ok dep ->
                    MessageBus.notify
                        Notification.Success
                        "Package Removed!"
                        "Ellie removed your package. Don't forget to remove it from your code!"

                Err apiError ->
                    MessageBus.notify
                        Notification.Error
                        "Couldn't Remove Package"
                        ("Elle couldn't remove your package. Here's what the server said:\n\n" ++ apiError.explanation)
            )

        _ ->
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
            model.session
                |> RemoteData.map (\_ -> model)
                |> RemoteData.withDefault { model | session = Loading, serverRevision = Loading, compileResult = NotAsked }

        SpecificRevision projectId revisionNumber ->
            let
                modelForSession =
                    model.session
                        |> RemoteData.map (\_ -> model)
                        |> RemoteData.withDefault { model | session = Loading }
            in
                model.clientRevision
                    |> (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                    |> Maybe.map (\( p, r ) -> p /= projectId || r /= revisionNumber)
                    |> Maybe.andThen boolToMaybe
                    |> Maybe.map (\() -> { modelForSession | serverRevision = Loading, compileResult = NotAsked })
                    |> Maybe.withDefault modelForSession
    , Cmd.batch
        [ cmd
        , case route of
            NotFound ->
                Navigation.modifyUrl (Routing.construct NewProject)

            NewProject ->
                Cmd.batch
                    [ model.session
                        |> RemoteData.map (\_ -> Cmd.none)
                        |> RemoteData.withDefault (Api.createNewSession |> Api.send CreateSessionCompleted)
                    , Api.defaultRevision
                        |> Api.send LoadRevisionCompleted
                    , MessageBus.notify
                        Notification.Info
                        "Setting up your session"
                        "Ellie is getting everything ready for you to compile code and install packages on the server."
                    ]

            SpecificRevision projectId revisionNumber ->
                model.clientRevision
                    |> (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                    |> Maybe.map (\( p, r ) -> p /= projectId || r /= revisionNumber)
                    |> Maybe.withDefault True
                    |> boolToMaybe
                    |> Maybe.map
                        (\() ->
                            Cmd.batch
                                [ Api.exactRevision projectId revisionNumber
                                    |> Api.send LoadRevisionCompleted
                                , model.session
                                    |> RemoteData.map Api.removeSession
                                    |> RemoteData.map (Api.send (\_ -> NoOp))
                                    |> RemoteData.withDefault Cmd.none
                                , Api.createSessionForRevision projectId revisionNumber
                                    |> Api.send CreateSessionCompleted
                                , MessageBus.notify
                                    Notification.Info
                                    "Setting up your session"
                                    "Ellie is getting everything ready for you to compile code and install packages on the server."
                                , MessageBus.notify
                                    Notification.Info
                                    "Loading your project"
                                    "Ellie is loading up the project and revision you requested."
                                ]
                        )
                    |> Maybe.withDefault Cmd.none
        ]
    )
