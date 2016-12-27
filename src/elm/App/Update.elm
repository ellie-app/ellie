module App.Update
    exposing
        ( update
        , initialize
        , onRouteChange
        , Msg(..)
        , NewPackageFlowMsg(..)
        )

import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Navigation
import List.Nonempty as Nonempty
import Types.Dependency as Dependency exposing (Dependency)
import Types.NewPackageFlow as NewPackageFlow exposing (NewPackageFlow)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.Session as Session exposing (Session)
import Types.CompileError as CompileError exposing (CompileError)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Notification as Notification exposing (Notification)
import Types.Identity as Identity exposing (Identity)
import App.Model as Model exposing (Model, Flags)
import App.Routing as Routing exposing (Route(..))
import Shared.Api as Api
import Shared.Constants as Constants
import Shared.MessageBus as MessageBus


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


type NewPackageFlowMsg
    = Started
    | SearchTermUpdated String
    | SearchCompleted String (Result ApiError (List PackageSearchResult))
    | PackageSelected PackageSearchResult
    | VersionSelected Int
    | InstallRequested
    | InstallCompleted Dependency (Result ApiError ())
    | Cancelled


type Msg
    = CreateSessionCompleted (Result ApiError Session)
    | RouteChanged Route
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
    | NewPackageFlowMsg NewPackageFlowMsg
    | RemoveDependencyRequested Dependency
    | RemoveDependencyCompleted (Result ApiError Dependency)
    | WindowUnloaded
    | NotificationReceived Notification
    | ToggleNotifications
    | ResultDragStarted
    | ResultDragged Position
    | ResultDragEnded
    | EditorDragStarted
    | EditorDragged Position
    | EditorDragEnded
    | WindowSizeChanged Size
    | TitleChanged String
    | DescriptionChanged String
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
            "Ellie is 100% ready to connect to the server."
    else
        MessageBus.notify
            Notification.Error
            "You're Offline!"
            "Ellie can't connect to the server right now, so we've disabled most features."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    Identity.create position
                        |> Identity.map (\p -> toFloat (p.y - Constants.headerHeight))
                        |> Identity.map (\h -> h / toFloat (model.windowSize.height - Constants.headerHeight))
                        |> Identity.fold (clamp 0.2 0.8)
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
                    Identity.create position
                        |> Identity.map (\p -> toFloat (p.x - Constants.sidebarWidth))
                        |> Identity.map (\w -> w / toFloat (model.windowSize.width - Constants.sidebarWidth))
                        |> Identity.fold (clamp 0.2 0.8)
              }
            , Cmd.none
            )

        ResultDragEnded ->
            ( { model | resultDragging = False }
            , Cmd.none
            )

        ToggleNotifications ->
            ( { model | notificationsOpen = not model.notificationsOpen }
            , Cmd.none
            )

        NotificationReceived notification ->
            ( { model
                | notifications = notification :: model.notifications
                , notificationsOpen = model.notificationsOpen || notification.level == Notification.Error
                , notificationsHighlight = notification.level == Notification.Error
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
                            ("Ellie couldn't set up a session for you right now. Here's what the server said: " ++ apiError.explanation)
                ]
            )

        LoadRevisionCompleted revisionResult ->
            ( { model
                | serverRevision = RemoteData.fromResult revisionResult
                , clientRevision = Result.withDefault model.clientRevision revisionResult
              }
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
                        ("Ellie couldn't load the project you asked for. Here's what the server said: " ++ apiError.explanation)
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        CompileRequested ->
            ( { model | compileResult = Loading }
            , Cmd.batch
                [ model.session
                    |> RemoteData.map (Api.compile model.clientRevision.elmCode model.clientRevision.htmlCode)
                    |> RemoteData.map (Api.send CompileCompleted)
                    |> RemoteData.withDefault Cmd.none
                , MessageBus.notify
                    Notification.Info
                    "Compilation started"
                    "Ellie is compiling your code. When it's done we'll show you the result below."
                ]
            )

        CompileCompleted compileResult ->
            ( { model
                | compileResult =
                    RemoteData.fromResult compileResult
                , firstCompileComplete =
                    model.firstCompileComplete
                        || resultIsSuccess compileResult
                , elmCodeChanged =
                    not (resultIsSuccess compileResult)
              }
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
                        ("Ellie couldn't even run the compiler. Here's what the server said: " ++ apiError.explanation)
            )

        ElmCodeChanged code ->
            ( model
                |> Model.updateClientRevision (\r -> { r | elmCode = code })
                |> (\m -> { m | elmCodeChanged = True })
            , Cmd.none
            )

        HtmlCodeChanged code ->
            ( model
                |> Model.updateClientRevision (\r -> { r | htmlCode = code })
            , Cmd.none
            )

        SaveRequested ->
            ( { model | saveState = Loading }
            , Cmd.batch
                [ saveProject model
                , MessageBus.notify
                    Notification.Info
                    "Saving Your Project"
                    "Ellie is saving your work! Any changes to code or settings since you last saved will be included."
                ]
            )

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
                            ("Ellie couldn't save your project. Here's what the server said: " ++ apiError.explanation)
                ]
            )

        OnlineChanged isOnline ->
            ( { model | isOnline = isOnline }
            , onlineNotification isOnline
            )

        FormattingRequested ->
            ( model
            , Cmd.batch
                [ Api.format model.clientRevision.elmCode
                    |> Api.send FormattingCompleted
                , MessageBus.notify
                    Notification.Info
                    "Formatting Your Code"
                    "Ellie has asked the server to format your code with elm-format."
                ]
            )

        FormattingCompleted result ->
            ( model
                |> Model.updateClientRevision (\r -> { r | elmCode = Result.withDefault r.elmCode result })
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
                        ("Ellie couldn't format your code. Here's what the server said: " ++ apiError.explanation)
            )

        RemoveDependencyRequested dependency ->
            ( model
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
            ( model
                |> Model.updateClientRevision
                    (\r ->
                        { r
                            | dependencies =
                                result
                                    |> Result.map (\d -> List.filter ((/=) d) r.dependencies)
                                    |> Result.withDefault r.dependencies
                        }
                    )
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
                        ("Elle couldn't remove your package. Here's what the server said: " ++ apiError.explanation)
            )

        NewPackageFlowMsg npfMsg ->
            case npfMsg of
                Started ->
                    ( { model | newPackageFlow = NewPackageFlow.PackageSearch "" [] }
                    , Cmd.none
                    )

                SearchTermUpdated searchTerm ->
                    ( { model
                        | newPackageFlow =
                            NewPackageFlow.updateSearchTerm searchTerm model.newPackageFlow
                      }
                    , Api.searchPackages Constants.elmVersion searchTerm
                        |> Api.send (SearchCompleted searchTerm >> NewPackageFlowMsg)
                    )

                SearchCompleted searchTerm result ->
                    ( { model
                        | newPackageFlow =
                            NewPackageFlow.receiveResultsForTerm
                                searchTerm
                                (Result.withDefault [] result)
                                model.newPackageFlow
                      }
                    , Cmd.none
                    )

                PackageSelected package ->
                    ( { model
                        | newPackageFlow =
                            NewPackageFlow.VersionSearch package (Nonempty.get 0 package.versions)
                      }
                    , Cmd.none
                    )

                VersionSelected index ->
                    ( { model
                        | newPackageFlow =
                            NewPackageFlow.selectVersionAtIndex index model.newPackageFlow
                      }
                    , Cmd.none
                    )

                InstallRequested ->
                    ( { model
                        | newPackageFlow =
                            model.newPackageFlow
                                |> NewPackageFlow.toDependency
                                |> Maybe.map (\d -> NewPackageFlow.Installation d Loading)
                                |> Maybe.withDefault model.newPackageFlow
                      }
                    , Maybe.map2 (,) (model.session |> RemoteData.toMaybe) (model.newPackageFlow |> NewPackageFlow.toDependency)
                        |> Maybe.map
                            (\( s, d ) ->
                                Cmd.batch
                                    [ Api.addDependencies s [ d ]
                                        |> Api.send (InstallCompleted d)
                                        |> Cmd.map NewPackageFlowMsg
                                    , MessageBus.notify
                                        Notification.Info
                                        "Installing Package"
                                        "Ellie is installing your package. Once it's installed and compiled you'll be able to use it in your code."
                                    ]
                            )
                        |> Maybe.withDefault Cmd.none
                    )

                InstallCompleted dep result ->
                    ( model
                        |> (\m -> { m | newPackageFlow = NewPackageFlow.NotSearching })
                        |> Model.updateClientRevision
                            (\r ->
                                { r
                                    | dependencies =
                                        r.dependencies ++ (result |> Result.map (\_ -> [ dep ]) |> Result.withDefault [])
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
                                ("Elle couldn't install your package. Here's what the server said: " ++ apiError.explanation)
                    )

                Cancelled ->
                    ( { model | newPackageFlow = NewPackageFlow.NotSearching }
                    , Cmd.none
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
