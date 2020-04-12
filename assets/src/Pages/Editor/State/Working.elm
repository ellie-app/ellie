module Pages.Editor.State.Working exposing
    ( ErrorsPane(..)
    , Model
    , Msg(..)
    , SuccessPane(..)
    , Workbench(..)
    , compilerVersion
    , hasChanged
    , init
    , subscriptions
    , update
    )

import BoundedDeque exposing (BoundedDeque)
import Data.Jwt exposing (Jwt)
import Data.Replaceable as Replaceable exposing (Replaceable)
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Ellie.Ui.CodeEditor as CodeEditor exposing (Located, Token)
import Elm.Compiler as Compiler
import Elm.Docs as Docs exposing (Module)
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Pages.Editor.Effects as Effects
import Pages.Editor.Route as Route
import Pages.Editor.State.Actions as Actions
import Pages.Editor.Types.Analysis as Analysis exposing (Analysis)
import Pages.Editor.Types.EditorAction as EditorAction exposing (EditorAction)
import Pages.Editor.Types.Log as Log exposing (Log)
import Pages.Editor.Types.Notification as Notification exposing (Notification)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)



-- MODEL


type Workbench
    = Ready
    | FinishedWithError
        { error : Error
        , pane : ErrorsPane
        }
    | Finished
        { logs : BoundedDeque Log
        , pane : SuccessPane
        , logSearch : String
        , canDebug : Bool
        , renderedHtml : String
        }


type ErrorsPane
    = ErrorsList
    | ErrorsShare


type SuccessPane
    = SuccessOutput
    | SuccessLogs
    | SuccessDebug
    | SuccessShare


type alias Model =
    { elmCode : String
    , htmlCode : String
    , packages : List Package
    , projectName : String
    , token : Jwt
    , recovery : Maybe Revision
    , defaultRevision : Revision
    , defaultPackages : List Package
    , revision : Replaceable Revision.Id Revision
    , saving : Bool
    , actions : Actions.Model
    , user : User
    , workbench : Workbench
    , compiling : Bool
    , connected : Bool
    , animating : Bool
    , workbenchRatio : Float
    , editorsRatio : Float
    , notifications : List Notification
    , analysis : Analysis
    }


reset : Jwt -> User -> Maybe Revision -> Revision.External -> List Package -> Model
reset token user recovery external defaultPackages =
    let
        activeRevision =
            case external of
                Revision.Default ->
                    Revision.default defaultPackages

                Revision.Example revision ->
                    revision

                Revision.Remote ( _, revision ) ->
                    revision

        savedRevision =
            case external of
                Revision.Remote ( id, r ) ->
                    Replaceable.Loaded ( id, r )

                _ ->
                    Replaceable.NotAsked

        isLatestElm =
            case external of
                Revision.Default ->
                    True

                Revision.Example revision ->
                    Version.compatible revision.elmVersion Compiler.version

                Revision.Remote ( _, revision ) ->
                    Version.compatible revision.elmVersion Compiler.version
    in
    { elmCode = activeRevision.elmCode
    , htmlCode = activeRevision.htmlCode
    , packages = activeRevision.packages
    , projectName = activeRevision.title
    , notifications = []
    , token = token
    , defaultRevision = activeRevision
    , defaultPackages = defaultPackages
    , revision = savedRevision
    , actions = Actions.Hidden
    , workbench = Ready
    , compiling = False
    , saving = False
    , connected = True
    , animating = True
    , user = user
    , workbenchRatio = 0.5
    , editorsRatio = 0.6
    , analysis = Analysis.empty
    , recovery = recovery
    }
        |> addNotificationIf (not isLatestElm)
            { title = "Outdated Code"
            , severity = Notification.Warning
            , message = "This code was written with an older version of the Elm compiler. You can still modify the code and compile it, but you cannot save your changes."
            , actions = []
            }
        |> addNotificationIf (recovery /= Nothing)
            { title = "Recover From Crash"
            , severity = Notification.Warning
            , message = "It looks like Ellie may have crashed before you had a chance to save your work. Do you want to recover what you were working on?"
            , actions = [ Notification.PerformAction "Recover" EditorAction.RecoverCrash ]
            }


addNotificationIf : Bool -> Notification -> Model -> Model
addNotificationIf cond notification model =
    if cond then
        addNotification notification model

    else
        model


addNotification : Notification -> Model -> Model
addNotification notification model =
    { model | notifications = notification :: model.notifications }


compilerVersion : Model -> Version
compilerVersion model =
    model.revision
        |> Replaceable.toMaybe
        |> Maybe.map (Tuple.second >> .elmVersion)
        |> Maybe.withDefault Compiler.version


toRevision : Model -> Revision
toRevision model =
    { elmCode = model.elmCode
    , htmlCode = model.htmlCode
    , packages = model.packages
    , title = model.projectName
    , elmVersion = Compiler.version
    }


hasChanged : Model -> Bool
hasChanged model =
    case Replaceable.toMaybe model.revision of
        Nothing ->
            (model.elmCode /= model.defaultRevision.elmCode)
                || (model.htmlCode /= model.defaultRevision.htmlCode)
                || (model.packages /= model.defaultRevision.packages)
                || (model.projectName /= model.defaultRevision.title)

        Just ( _, revision ) ->
            (model.elmCode /= revision.elmCode)
                || (model.htmlCode /= revision.htmlCode)
                || (model.packages /= revision.packages)
                || (model.projectName /= revision.title)


canReplaceRevision : Revision.Id -> Model -> Bool
canReplaceRevision revisionId model =
    case model.revision of
        Replaceable.NotAsked ->
            True

        Replaceable.Loading rid ->
            revisionId == rid

        Replaceable.Replacing rid _ ->
            revisionId == rid

        Replaceable.Loaded ( rid, _ ) ->
            False



-- UPDATE


type Msg
    = -- Editor stuff
      ElmCodeChanged String
    | HtmlCodeChanged String
    | FormatRequested
    | FormatCompleted (Result () String)
    | CollapseHtml
    | EditorsResized Float
    | TokenChanged (Located Token)
    | DocsReceived (List Module)
      -- Action stuff
    | SettingsChanged User.Settings
    | ChangedProjectName String
    | PackageInstalled Package
    | PackageUninstalled Package
    | ActionPaneSelected Actions.Model
    | ActionsMsg Actions.Msg
      -- Workbench stuff
    | CompileRequested
    | CompileFailed
    | ExpandWorkbench
    | CompileFinished (Maybe Error)
    | WorkbenchResized Float
    | ErrorsPaneSelected ErrorsPane
    | SuccessPaneSelected SuccessPane
    | IframeReloadClicked
    | ClearLogsClicked
    | LogReceived Log
    | LogSearchChanged String
    | LocationSelected Error.Position
    | SaveRequested
    | SaveCompleted (Result () ( Revision.Id, Revision ))
    | CanDebugUpdated Bool
    | RuntimExceptionOccured String
      -- Share stuff
    | DownloadZip
      -- StatusBar stuff
    | CloseNotification Notification
    | CloseAllNotifications
      -- Global stuff
    | RouteChanged Route.Route
    | RevisionLoaded Revision.Id (Result () Revision)
    | AnimationFinished
    | OnlineStatusChanged Bool
    | CrashRecovered Revision
    | EditorActionPerformed EditorAction
    | NoOp


init : Jwt -> User -> Maybe Revision -> Revision.External -> List Package -> ( Model, Command Msg )
init token user recovery revision defaultPackages =
    reset token user recovery revision defaultPackages
        |> update CompileRequested
        |> (\( model, outbound ) ->
                ( model
                , Command.batch
                    [ outbound
                    , Effects.delay 990
                        |> Command.map (\_ -> AnimationFinished)
                    , Effects.getDocs model.packages
                        |> Command.map DocsReceived
                    ]
                )
           )
        |> withRecoveryUpdate


update : Msg -> Model -> ( Model, Command Msg )
update msg ({ user } as model) =
    withRecoveryUpdate <|
        case msg of
            RuntimExceptionOccured message ->
                ( model
                    |> addNotification
                        { title = "Exception Thrown in Output"
                        , severity = Notification.Failure
                        , message = message
                        , actions = []
                        }
                , Command.none
                )

            EditorActionPerformed action ->
                update (fromEditorAction model action) model

            DocsReceived modules ->
                ( { model | analysis = Analysis.withModules modules model.analysis }
                , Command.none
                )

            TokenChanged token ->
                ( { model
                    | analysis =
                        model.analysis
                            |> Analysis.withCode model.elmCode
                            |> Analysis.withToken token
                  }
                , Command.none
                )

            SaveRequested ->
                case model.user.acceptedTerms of
                    Just acceptedTerms ->
                        ( { model | saving = True }
                        , model
                            |> toRevision
                            |> Effects.createRevision model.token acceptedTerms
                            |> Command.map (Result.mapError (\_ -> ()))
                            |> Command.map (Result.map (\rid -> ( rid, toRevision model )))
                            |> Command.map SaveCompleted
                        )

                    Nothing ->
                        ( model, Command.none )

            SaveCompleted (Ok ( revisionId, revision )) ->
                case Replaceable.loading model.revision of
                    Just key ->
                        if key == revisionId then
                            ( { model | saving = False, revision = Replaceable.Loaded ( revisionId, revision ) }
                            , Command.none
                            )

                        else
                            ( { model | saving = False }
                            , Command.none
                            )

                    Nothing ->
                        ( { model
                            | saving = False
                            , revision = Replaceable.Loaded ( revisionId, revision )
                            , notifications =
                                { title = "Saved!"
                                , severity = Notification.Success
                                , message = "Your changes have been saved. You can share them using this link:"
                                , actions = [ Notification.CopyLink <| Revision.editorLink revisionId ]
                                }
                                    :: model.notifications
                          }
                        , Effects.navigate <| Route.toString <| Route.Existing revisionId
                        )

            SaveCompleted (Err _) ->
                ( { model
                    | saving = False
                    , revision = Replaceable.reset model.revision
                  }
                    |> addNotification
                        { title = "Saving failed"
                        , severity = Notification.Failure
                        , message = "Your changes could not be saved at this time. Saving should always work, so the maintainers have been notified of this issue."
                        , actions = []
                        }
                , Command.none
                )

            LocationSelected location ->
                ( model
                , Effects.moveElmCursor location
                )

            DownloadZip ->
                ( model
                , Effects.downloadZip
                    model.elmCode
                    model.htmlCode
                    { sourceDirs = []
                    , deps = model.packages
                    , elm = Compiler.version
                    }
                )

            CloseNotification notification ->
                ( { model
                    | notifications = List.filter (Notification.eq notification >> not) model.notifications
                  }
                , Command.none
                )

            CloseAllNotifications ->
                ( { model | notifications = [] }
                , Command.none
                )

            ErrorsPaneSelected pane ->
                case model.workbench of
                    FinishedWithError state ->
                        ( { model | workbench = FinishedWithError { state | pane = pane } }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            SuccessPaneSelected pane ->
                case model.workbench of
                    Finished state ->
                        ( { model | workbench = Finished { state | pane = pane } }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            ExpandWorkbench ->
                if model.workbenchRatio < 0.05 then
                    ( { model | workbenchRatio = 0.5 }
                    , Command.none
                    )

                else
                    ( { model | workbenchRatio = 0 }
                    , Command.none
                    )

            LogSearchChanged logSearch ->
                case model.workbench of
                    Finished state ->
                        ( { model | workbench = Finished { state | logSearch = logSearch } }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            LogReceived log ->
                case model.workbench of
                    Finished state ->
                        ( { model
                            | workbench =
                                Finished
                                    { state
                                        | logs = BoundedDeque.pushFront log state.logs
                                        , logSearch = ""
                                    }
                          }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            ClearLogsClicked ->
                case model.workbench of
                    Finished state ->
                        ( { model
                            | workbench =
                                Finished
                                    { state
                                        | logs = BoundedDeque.empty 50
                                        , logSearch = ""
                                    }
                          }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            CanDebugUpdated canDebug ->
                case model.workbench of
                    Finished state ->
                        ( { model
                            | workbench = Finished { state | canDebug = canDebug }
                          }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            IframeReloadClicked ->
                ( model
                , Effects.reloadOutput
                )

            CompileRequested ->
                if model.compiling then
                    ( model, Command.none )

                else
                    ( { model | compiling = True }
                    , Effects.compile model.token (compilerVersion model) model.elmCode model.packages
                        |> Command.map
                            (\result ->
                                case result of
                                    Ok _ ->
                                        NoOp

                                    Err _ ->
                                        CompileFailed
                            )
                    )

            CompileFailed ->
                ( { model | compiling = False }
                    |> addNotification
                        { title = "Compiling failed"
                        , message = "Ellie was unable to compile your code. Compiling should always work, so this has been automatically reported to the maintainers."
                        , severity = Notification.Failure
                        , actions = []
                        }
                , Command.none
                )

            CompileFinished error ->
                case ( model.compiling, error, model.workbench ) of
                    ( True, Nothing, Finished state ) ->
                        ( { model
                            | compiling = False
                            , workbench =
                                Finished
                                    { state
                                        | logs = BoundedDeque.empty 50
                                        , logSearch = ""
                                        , renderedHtml = model.htmlCode
                                    }
                          }
                        , Effects.reloadOutput
                        )

                    ( True, Nothing, _ ) ->
                        ( { model
                            | compiling = False
                            , workbench =
                                Finished
                                    { logs = BoundedDeque.empty 50
                                    , pane = SuccessOutput
                                    , logSearch = ""
                                    , canDebug = True
                                    , renderedHtml = model.htmlCode
                                    }
                          }
                        , Effects.reloadOutput
                        )

                    ( True, Just e, FinishedWithError state ) ->
                        ( { model
                            | compiling = False
                            , workbench = FinishedWithError { state | error = e }
                          }
                        , Command.none
                        )

                    ( True, Just e, _ ) ->
                        ( { model
                            | compiling = False
                            , workbench = FinishedWithError { error = e, pane = ErrorsList }
                          }
                        , Command.none
                        )

                    _ ->
                        ( model, Command.none )

            FormatRequested ->
                ( model
                , Effects.formatCode model.token (compilerVersion model) model.elmCode
                    |> Command.map (Result.mapError (\_ -> ()))
                    |> Command.map FormatCompleted
                )

            FormatCompleted (Ok code) ->
                ( { model | elmCode = code }
                , Command.none
                )

            FormatCompleted (Err _) ->
                ( model
                    |> addNotification
                        { title = "Formatting failed"
                        , severity = Notification.Failure
                        , message = "The server couldn't format your code. Formatting should always work, so the maintainers have been notified of this issue."
                        , actions = []
                        }
                , Command.none
                )

            CollapseHtml ->
                ( if model.editorsRatio == 1 then
                    { model | editorsRatio = 0.75 }

                  else
                    { model | editorsRatio = 1 }
                , Command.none
                )

            PackageInstalled package ->
                let
                    nextPackages =
                        model.packages ++ [ package ]
                in
                ( { model | packages = nextPackages }
                , Effects.getDocs nextPackages
                    |> Command.map DocsReceived
                )

            PackageUninstalled package ->
                let
                    nextPackages =
                        List.filter ((/=) package) model.packages
                in
                ( { model | packages = nextPackages }
                , Effects.getDocs nextPackages
                    |> Command.map DocsReceived
                )

            ChangedProjectName projectName ->
                ( { model | projectName = projectName }
                , Command.none
                )

            WorkbenchResized ratio ->
                ( { model | workbenchRatio = ratio }
                , Command.none
                )

            EditorsResized ratio ->
                ( { model | editorsRatio = ratio }
                , Command.none
                )

            ActionPaneSelected actions ->
                ( { model | actions = actions }
                , Command.none
                )

            SettingsChanged settings ->
                ( { model | user = { user | settings = settings } }
                , Effects.updateUser { user | settings = settings }
                )

            AnimationFinished ->
                ( { model | animating = False }
                , Command.none
                )

            OnlineStatusChanged connected ->
                ( { model | connected = connected }
                , Command.none
                )

            NoOp ->
                ( model
                , Command.none
                )

            ActionsMsg actionsMsg ->
                Actions.update actionsMsg model.actions
                    |> Tuple.mapFirst (\a -> { model | actions = a })
                    |> Tuple.mapSecond (Command.map ActionsMsg)

            ElmCodeChanged code ->
                ( { model | elmCode = code }
                , Command.none
                )

            HtmlCodeChanged code ->
                ( { model | htmlCode = code }
                , Command.none
                )

            CrashRecovered revision ->
                ( { model
                    | elmCode = revision.elmCode
                    , htmlCode = revision.htmlCode
                    , packages = revision.packages
                    , projectName = revision.title
                    , notifications = List.filter (\n -> n.title /= "Recover From Crash") model.notifications
                  }
                , Command.none
                )

            RevisionLoaded revisionId result ->
                case ( model.revision, result ) of
                    ( Replaceable.Loading rid, Ok revision ) ->
                        if rid == revisionId then
                            ( reset
                                model.token
                                model.user
                                model.recovery
                                (Revision.Remote ( revisionId, revision ))
                                model.defaultPackages
                            , Command.none
                            )

                        else
                            ( model, Command.none )

                    ( Replaceable.Replacing rid _, Ok revision ) ->
                        if rid == revisionId then
                            ( reset
                                model.token
                                model.user
                                model.recovery
                                (Revision.Remote ( revisionId, revision ))
                                model.defaultPackages
                            , Command.none
                            )

                        else
                            ( model, Command.none )

                    ( Replaceable.Loading _, Err _ ) ->
                        ( model
                            |> addNotification
                                { title = "Ellie not found"
                                , severity = Notification.Failure
                                , message = "The Ellie you asked for couldn't be loaded."
                                , actions = []
                                }
                        , Effects.redirect <| Route.toString Route.New
                        )

                    ( Replaceable.Replacing _ ( rid, _ ), Err _ ) ->
                        ( model
                            |> addNotification
                                { title = "Ellie not found"
                                , severity = Notification.Failure
                                , message = "The Ellie you asked for couldn't be loaded."
                                , actions = []
                                }
                        , Effects.redirect <| Route.toString <| Route.Existing rid
                        )

                    _ ->
                        ( model, Command.none )

            RouteChanged route ->
                case route of
                    Route.Existing newRevisionId ->
                        case model.revision of
                            Replaceable.Loaded ( rid, r ) ->
                                if newRevisionId /= rid then
                                    ( { model | revision = Replaceable.Replacing newRevisionId ( rid, r ) }
                                    , Effects.getRevision newRevisionId
                                        |> Command.map (Result.mapError (\_ -> ()))
                                        |> Command.map (RevisionLoaded newRevisionId)
                                    )

                                else
                                    ( model, Command.none )

                            Replaceable.Loading rid ->
                                if newRevisionId /= rid then
                                    ( { model | revision = Replaceable.Loading newRevisionId }
                                    , Effects.getRevision newRevisionId
                                        |> Command.map (Result.mapError (\_ -> ()))
                                        |> Command.map (RevisionLoaded newRevisionId)
                                    )

                                else
                                    ( model, Command.none )

                            Replaceable.Replacing rid entity ->
                                if newRevisionId /= rid then
                                    ( { model | revision = Replaceable.Replacing newRevisionId entity }
                                    , Effects.getRevision newRevisionId
                                        |> Command.map (Result.mapError (\_ -> ()))
                                        |> Command.map (RevisionLoaded newRevisionId)
                                    )

                                else
                                    ( model, Command.none )

                            Replaceable.NotAsked ->
                                ( { model | revision = Replaceable.Loading newRevisionId }
                                , Effects.getRevision newRevisionId
                                    |> Command.map (Result.mapError (\_ -> ()))
                                    |> Command.map (RevisionLoaded newRevisionId)
                                )

                    Route.New ->
                        case model.revision of
                            Replaceable.NotAsked ->
                                ( model, Command.none )

                            _ ->
                                ( reset
                                    model.token
                                    model.user
                                    model.recovery
                                    Revision.Default
                                    model.defaultPackages
                                , Command.none
                                )

                    Route.Example revision ->
                        ( reset
                            model.token
                            model.user
                            model.recovery
                            (Revision.Example revision)
                            model.defaultPackages
                        , Effects.redirect <| Route.toString Route.New
                        )

                    Route.NotFound ->
                        case Replaceable.toMaybe model.revision of
                            Just ( rid, _ ) ->
                                ( model
                                , Effects.redirect <| Route.toString <| Route.Existing rid
                                )

                            Nothing ->
                                ( model
                                , Effects.redirect <| Route.toString Route.New
                                )


withRecoveryUpdate : ( Model, Command Msg ) -> ( Model, Command Msg )
withRecoveryUpdate ( model, command ) =
    ( model
    , Command.batch
        [ command
        , Effects.updateRecoveryRevision <|
            if hasChanged model then
                Just (toRevision model)

            else
                Nothing
        ]
    )


subscriptions : Model -> Subscription Msg
subscriptions model =
    Subscription.batch
        [ Actions.subscriptions model.actions
            |> Subscription.map ActionsMsg
        , Effects.workspaceUpdates model.token
            |> Subscription.map
                (\updates ->
                    case updates of
                        WorkspaceUpdate.CompileCompleted maybeError ->
                            CompileFinished maybeError

                        WorkspaceUpdate.Connected ->
                            OnlineStatusChanged True

                        WorkspaceUpdate.Disconnected ->
                            OnlineStatusChanged False

                        _ ->
                            NoOp
                )
        , Effects.networkStatus
            |> Subscription.map
                (\online ->
                    if online == model.connected then
                        NoOp

                    else
                        OnlineStatusChanged online
                )
        , Subscription.map EditorActionPerformed Effects.keyCombos
        ]


fromEditorAction : Model -> EditorAction -> Msg
fromEditorAction model action =
    case action of
        EditorAction.Save ->
            if model.connected && hasChanged model then
                SaveRequested

            else
                NoOp

        EditorAction.Recompile ->
            if model.compiling then
                NoOp

            else
                CompileRequested

        EditorAction.OpenDebugger ->
            case model.workbench of
                Finished state ->
                    if state.canDebug then
                        SuccessPaneSelected SuccessDebug

                    else
                        NoOp

                _ ->
                    NoOp

        EditorAction.OpenLogs ->
            case model.workbench of
                Finished state ->
                    SuccessPaneSelected SuccessLogs

                _ ->
                    NoOp

        EditorAction.OpenOutput ->
            case model.workbench of
                Finished _ ->
                    SuccessPaneSelected SuccessOutput

                FinishedWithError _ ->
                    ErrorsPaneSelected ErrorsList

                _ ->
                    NoOp

        EditorAction.OpenPackages ->
            ActionPaneSelected Actions.packages

        EditorAction.OpenSettings ->
            ActionPaneSelected Actions.Settings

        EditorAction.ReloadOutput ->
            case model.workbench of
                Finished _ ->
                    IframeReloadClicked

                _ ->
                    NoOp

        EditorAction.RecoverCrash ->
            case model.recovery of
                Just recovery ->
                    CrashRecovered recovery

                Nothing ->
                    NoOp
