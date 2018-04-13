module Pages.Editor.State.Working exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Data.Either as Either exposing (Either(..))
import Data.Jwt exposing (Jwt)
import Data.Replaceable as Replaceable exposing (Replaceable)
import Data.Uuid as Uuid exposing (Uuid)
import Elm.Compiler as Compiler
import Elm.Docs as Docs exposing (Module)
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Extra.Maybe as Maybe
import Pages.Editor.Effects.Exception as Exception exposing (Exception)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route
import Pages.Editor.State.Actions as Actions
import Pages.Editor.Types.Analysis as Analysis exposing (Analysis)
import Pages.Editor.Types.Example as Example exposing (Example)
import Pages.Editor.Types.Log as Log exposing (Log)
import Pages.Editor.Types.Notification as Notification exposing (Notification)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.Settings as Settings exposing (Settings)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)


-- MODEL


type Workbench
    = Ready
    | FinishedWithError { error : Error, pane : ErrorsPane }
    | Finished { logs : BoundedDeque Log, pane : SuccessPane, logSearch : String }


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
    , activeExample : Example
    , defaultPackages : List Package
    , revision : Replaceable RevisionId Revision
    , saving : Bool
    , actions : Actions.Model
    , user : User
    , workbench : Workbench
    , creatingGist : Bool
    , compiling : Bool
    , connected : Bool
    , animating : Bool
    , workbenchRatio : Float
    , editorsRatio : Float
    , notifications : List Notification
    , analysis : Analysis
    }


reset : Jwt -> User -> Maybe ( RevisionId, Revision ) -> List Package -> Model
reset token user revision defaultPackages =
    { elmCode =
        revision
            |> Maybe.map (Tuple.second >> .elmCode)
            |> Maybe.withDefault (.elm Example.helloWorld)
    , htmlCode =
        revision
            |> Maybe.map (Tuple.second >> .htmlCode)
            |> Maybe.withDefault (.html Example.helloWorld)
    , packages =
        revision
            |> Maybe.map (Tuple.second >> .packages)
            |> Maybe.withDefault defaultPackages
    , activeExample = Example.helloWorld
    , projectName = ""
    , token = token
    , defaultPackages = defaultPackages
    , revision = Replaceable.fromMaybe revision
    , actions = Actions.Hidden
    , workbench = Ready
    , creatingGist = False
    , compiling = False
    , saving = False
    , connected = True
    , animating = True
    , user = user
    , workbenchRatio = 0.5
    , editorsRatio = 0.75
    , notifications = []
    , analysis = Analysis.empty
    }


ownRevision : Model -> Bool
ownRevision model =
    case Replaceable.toMaybe model.revision of
        Just revision ->
            -- Maybe.eq
            --     (Tuple.second revision |> .userId)
            --     (Just (Entity.key model.user))
            -- TODO
            False

        Nothing ->
            False


toRevision : Model -> Revision
toRevision model =
    { elmCode = model.elmCode
    , htmlCode = model.htmlCode
    , packages = model.packages
    , title = model.projectName
    , elmVersion = Compiler.version
    }


toSave : Model -> Either Revision ( RevisionId, Revision )
toSave model =
    case Replaceable.toMaybe model.revision of
        Just ( rid, _ ) ->
            if ownRevision model then
                Right
                    ( rid, toRevision model )
            else
                Left <| toRevision model

        Nothing ->
            Left <| toRevision model


shouldCheckNavigation : Model -> Bool
shouldCheckNavigation model =
    case Replaceable.toMaybe model.revision of
        Nothing ->
            (model.elmCode /= model.activeExample.elm)
                || (model.htmlCode /= model.activeExample.html)
                || (model.packages /= model.defaultPackages)

        Just ( _, revision ) ->
            (model.elmCode /= revision.elmCode)
                || (model.htmlCode /= revision.htmlCode)
                || (model.packages /= revision.packages)



-- UPDATE


type Msg
    = -- Editor stuff
      ElmCodeChanged String
    | HtmlCodeChanged String
    | FormatRequested
    | FormatCompleted String
    | CollapseHtml
    | EditorsResized Float
    | EditorSettled
    | ExampleSelected Example
    | TokenChanged String
    | DocsReceived (List Module)
      -- Action stuff
    | SettingsChanged Settings
    | ChangedProjectName String
    | PackageInstalled Package
    | PackageUninstalled Package
    | ActionPaneSelected Actions.Model
    | ActionsMsg Actions.Msg
      -- Workbench stuff
    | CompileRequested
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
    | SaveCompleted RevisionId Revision
      -- Share stuff
    | CreateGistRequested
    | CreateGistComplete (Maybe String)
    | DownloadZip
      -- StatusBar stuff
    | CloseNotification Notification
    | CloseAllNotifications
      -- Exception Stuff
    | ExceptionReceived Exception
      -- Global stuff
    | RouteChanged Route.Route
    | RevisionLoaded RevisionId Revision
    | AnimationFinished
    | OnlineStatusChanged Bool
    | NoOp


init : Jwt -> User -> Maybe ( RevisionId, Revision ) -> List Package -> ( Model, Outbound Msg )
init token user revision defaultPackages =
    reset token user revision defaultPackages
        |> update CompileRequested
        |> (\( model, outbound ) ->
                ( model
                , Outbound.batch
                    [ outbound
                    , Outbound.Delay 1000 AnimationFinished

                    -- , Outbound.GetDocs model.packages DocsReceived
                    ]
                )
           )


update : Msg -> Model -> ( Model, Outbound Msg )
update msg ({ user } as model) =
    case msg of
        EditorSettled ->
            ( { model | analysis = Analysis.withCode model.elmCode model.analysis }
            , Outbound.none
            )

        DocsReceived modules ->
            ( { model | analysis = Analysis.withModules modules model.analysis }
            , Outbound.none
            )

        TokenChanged token ->
            ( model, Outbound.none )

        SaveRequested ->
            case toSave model of
                Left revision ->
                    ( { model | saving = True }
                    , Outbound.CreateRevision model.token revision SaveCompleted
                    )

                Right ( revisionId, revision ) ->
                    ( { model | saving = True }
                    , Outbound.UpdateRevision model.token revisionId.projectId revision SaveCompleted
                    )

        SaveCompleted revisionId revision ->
            case Replaceable.loading model.revision of
                Just key ->
                    if RevisionId.eq key revisionId then
                        ( { model | saving = False, revision = Replaceable.Loaded ( revisionId, revision ) }
                        , Outbound.none
                        )
                    else
                        ( { model | saving = False }
                        , Outbound.none
                        )

                Nothing ->
                    ( { model
                        | saving = False
                        , revision = Replaceable.Loaded ( revisionId, revision )
                        , notifications =
                            { title = "Saved!"
                            , severity = Notification.Success
                            , message = "Your changes have been saved. You can share them using this link:"
                            , actions = [ Notification.CopyLink <| RevisionId.link revisionId ]
                            }
                                :: model.notifications
                      }
                    , Outbound.Navigate <| Route.toString <| Route.Existing revisionId
                    )

        LocationSelected location ->
            ( model
            , Outbound.MoveElmCursor location
            )

        DownloadZip ->
            ( model
            , Outbound.DownloadZip
                { elm = model.elmCode
                , html = model.htmlCode
                , project =
                    { sourceDirs = []
                    , deps = model.packages
                    , elm = { major = 0, minor = 19, patch = 0 }
                    }
                }
            )

        CreateGistRequested ->
            ( { model | creatingGist = True }
            , Outbound.CreateGist
                { title = model.projectName
                , elm = model.elmCode
                , html = model.htmlCode
                , project =
                    { sourceDirs = []
                    , deps = model.packages
                    , elm = { major = 0, minor = 19, patch = 0 }
                    }
                }
                CreateGistComplete
            )

        CreateGistComplete (Just url) ->
            ( { model
                | creatingGist = False
                , notifications =
                    { title = "Gist Created"
                    , severity = Notification.Success
                    , message = "We created a Gist from your project. If you have popups blocked you can go directly to"
                    , actions = [ Notification.GoToLink url ]
                    }
                        :: model.notifications
              }
            , Outbound.OpenInNewTab url
            )

        CreateGistComplete Nothing ->
            ( { model | creatingGist = False }
            , Outbound.none
            )

        ExceptionReceived exception ->
            let
                notification =
                    Notification.fromException exception
            in
            ( { model
                | notifications = notification :: List.filter (Notification.eq notification >> not) model.notifications
              }
            , Outbound.none
            )

        CloseNotification notification ->
            ( { model
                | notifications = List.filter (Notification.eq notification >> not) model.notifications
              }
            , Outbound.none
            )

        CloseAllNotifications ->
            ( { model | notifications = [] }
            , Outbound.none
            )

        ErrorsPaneSelected pane ->
            case model.workbench of
                FinishedWithError state ->
                    ( { model | workbench = FinishedWithError { state | pane = pane } }
                    , Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

        SuccessPaneSelected pane ->
            case model.workbench of
                Finished state ->
                    ( { model | workbench = Finished { state | pane = pane } }
                    , case pane of
                        SuccessOutput ->
                            Outbound.SwitchToProgram

                        SuccessDebug ->
                            Outbound.SwitchToDebugger

                        _ ->
                            Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

        ExpandWorkbench ->
            if model.workbenchRatio < 0.05 then
                ( { model | workbenchRatio = 0.5 }
                , Outbound.none
                )
            else
                ( { model | workbenchRatio = 0 }
                , Outbound.none
                )

        LogSearchChanged logSearch ->
            case model.workbench of
                Finished state ->
                    ( { model | workbench = Finished { state | logSearch = logSearch } }
                    , Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

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
                    , Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

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
                    , Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

        IframeReloadClicked ->
            ( model
            , Outbound.ReloadIframe
            )

        CompileRequested ->
            ( { model | compiling = True }
            , Outbound.Compile model.token model.elmCode model.packages
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
                                }
                      }
                    , Outbound.ReloadIframe
                    )

                ( True, Nothing, _ ) ->
                    ( { model
                        | compiling = False
                        , workbench =
                            Finished
                                { logs = BoundedDeque.empty 50
                                , pane = SuccessOutput
                                , logSearch = ""
                                }
                      }
                    , Outbound.ReloadIframe
                    )

                ( True, Just error, FinishedWithError state ) ->
                    ( { model
                        | compiling = False
                        , workbench = FinishedWithError { state | error = error }
                      }
                    , Outbound.none
                    )

                ( True, Just error, _ ) ->
                    ( { model
                        | compiling = False
                        , workbench = FinishedWithError { error = error, pane = ErrorsList }
                      }
                    , Outbound.none
                    )

                _ ->
                    ( model, Outbound.none )

        FormatRequested ->
            ( model
            , Outbound.FormatElmCode model.elmCode FormatCompleted
            )

        FormatCompleted code ->
            ( { model | elmCode = code }
            , Outbound.none
            )

        CollapseHtml ->
            ( if model.editorsRatio == 1 then
                { model | editorsRatio = 0.75 }
              else
                { model | editorsRatio = 1 }
            , Outbound.none
            )

        ExampleSelected example ->
            let
                nextModel =
                    { model
                        | activeExample = example
                        , elmCode = example.elm
                        , htmlCode = example.html
                    }
            in
            ( nextModel
            , Outbound.EnableNavigationCheck <| shouldCheckNavigation nextModel
            )

        PackageInstalled package ->
            let
                nextPackages =
                    model.packages ++ [ package ]
            in
            ( { model | packages = nextPackages }
              -- , Outbound.GetDocs nextPackages DocsReceived
            , Outbound.none
            )

        PackageUninstalled package ->
            let
                nextPackages =
                    List.filter ((/=) package) model.packages
            in
            ( { model | packages = nextPackages }
              -- , Outbound.GetDocs nextPackages DocsReceived
            , Outbound.none
            )

        ChangedProjectName projectName ->
            ( { model | projectName = projectName }
            , Outbound.none
            )

        WorkbenchResized ratio ->
            ( { model | workbenchRatio = ratio }
            , Outbound.none
            )

        EditorsResized ratio ->
            ( { model | editorsRatio = ratio }
            , Outbound.none
            )

        ActionPaneSelected actions ->
            ( { model | actions = actions }
            , Outbound.none
            )

        SettingsChanged settings ->
            ( { model | user = { user | settings = settings } }
            , Outbound.SaveSettings model.token settings
            )

        AnimationFinished ->
            ( { model | animating = False }, Outbound.none )

        OnlineStatusChanged connected ->
            ( { model | connected = connected }
            , Outbound.none
            )

        NoOp ->
            ( model, Outbound.none )

        ActionsMsg actionsMsg ->
            Actions.update actionsMsg model.actions
                |> Tuple.mapFirst (\a -> { model | actions = a })
                |> Tuple.mapSecond (Outbound.map ActionsMsg)

        ElmCodeChanged code ->
            ( { model | elmCode = code }
            , Outbound.EnableNavigationCheck <| shouldCheckNavigation model
            )

        HtmlCodeChanged code ->
            ( { model | htmlCode = code }
            , Outbound.EnableNavigationCheck <| shouldCheckNavigation model
            )

        RevisionLoaded revisionId revision ->
            case model.revision of
                Replaceable.Loading rid ->
                    if rid == revisionId then
                        ( reset model.token model.user (Just ( revisionId, revision )) model.defaultPackages
                        , Outbound.none
                        )
                    else
                        ( model, Outbound.none )

                Replaceable.Replacing rid _ ->
                    if rid == revisionId then
                        ( reset model.token model.user (Just ( revisionId, revision )) model.defaultPackages
                        , Outbound.none
                        )
                    else
                        ( model, Outbound.none )

                _ ->
                    ( model, Outbound.none )

        RouteChanged route ->
            case route of
                Route.Existing newRevisionId ->
                    case model.revision of
                        Replaceable.Loaded ( rid, r ) ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Replacing newRevisionId ( rid, r ) }
                                , Outbound.GetRevision newRevisionId (RevisionLoaded newRevisionId)
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.Loading rid ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Loading newRevisionId }
                                , Outbound.GetRevision newRevisionId (RevisionLoaded newRevisionId)
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.Replacing rid entity ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Replacing newRevisionId entity }
                                , Outbound.GetRevision newRevisionId (RevisionLoaded newRevisionId)
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.NotAsked ->
                            ( { model | revision = Replaceable.Loading newRevisionId }
                            , Outbound.GetRevision newRevisionId (RevisionLoaded newRevisionId)
                            )

                Route.New ->
                    case model.revision of
                        Replaceable.NotAsked ->
                            ( model, Outbound.none )

                        _ ->
                            ( reset model.token model.user Nothing model.defaultPackages
                            , Outbound.none
                            )

                Route.NotFound ->
                    case Replaceable.toMaybe model.revision of
                        Just ( rid, _ ) ->
                            ( model
                            , Outbound.Redirect <| Route.toString <| Route.Existing rid
                            )

                        Nothing ->
                            ( model
                            , Outbound.Redirect <| Route.toString Route.New
                            )


subscriptions : Model -> Inbound Msg
subscriptions model =
    Inbound.batch
        [ Inbound.map ActionsMsg <| Actions.subscriptions model.actions
        , Inbound.WorkspaceUpdates model.token <|
            \update ->
                case update of
                    WorkspaceUpdate.CompileCompleted maybeError ->
                        CompileFinished maybeError

                    WorkspaceUpdate.Attached _ ->
                        OnlineStatusChanged True

                    _ ->
                        NoOp
        ]
