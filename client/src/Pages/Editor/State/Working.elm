module Pages.Editor.State.Working exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Data.Entity as Entity exposing (Entity(..))
import Data.Jwt exposing (Jwt)
import Data.Replaceable as Replaceable exposing (Replaceable)
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.User as User exposing (User)
import Elm.Compiler.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Pages.Editor.Effects.Exception as Exception exposing (Exception)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Route as Route
import Pages.Editor.State.Actions as Actions
import Pages.Editor.Types.Example as Example exposing (Example)
import Pages.Editor.Types.Log as Log exposing (Log)


type Workbench
    = Ready
    | FinishedWithErrors { errors : List Error, pane : ErrorsPane }
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
    , revision : Replaceable Revision.Id Revision
    , actions : Actions.Model
    , user : User
    , workbench : Workbench
    , compiling : Bool
    , connected : Bool
    , animating : Bool
    , workbenchRatio : Float
    , editorsRatio : Float
    , exceptions : List Exception
    }


init : Jwt -> User -> Maybe (Entity Revision.Id Revision) -> List Package -> ( Model, Outbound Msg )
init token user revision defaultPackages =
    ( { elmCode =
            revision
                |> Maybe.map (Entity.record >> .elmCode)
                |> Maybe.withDefault (.elm Example.helloWorld)
      , htmlCode =
            revision
                |> Maybe.map (Entity.record >> .htmlCode)
                |> Maybe.withDefault (.html Example.helloWorld)
      , packages =
            revision
                |> Maybe.map (Entity.record >> .packages)
                |> Maybe.withDefault defaultPackages
      , activeExample = Example.helloWorld
      , projectName = ""
      , token = token
      , defaultPackages = defaultPackages
      , revision = Replaceable.fromMaybe revision
      , actions = Actions.Hidden
      , workbench = Ready
      , compiling = False
      , connected = True
      , animating = True
      , user = user
      , workbenchRatio = 0.5
      , editorsRatio = 0.75
      , exceptions = []
      }
    , Outbound.Delay 1000 AnimationFinished
    )


shouldCheckNavigation : Model -> Bool
shouldCheckNavigation model =
    case Replaceable.toMaybe model.revision of
        Nothing ->
            (model.elmCode /= model.activeExample.elm)
                || (model.htmlCode /= model.activeExample.html)
                || (model.packages /= model.defaultPackages)

        Just (Entity _ revision) ->
            (model.elmCode /= revision.elmCode)
                || (model.htmlCode /= revision.htmlCode)
                || (model.packages /= revision.packages)


type Msg
    = -- Editor stuff
      ElmCodeChanged String
    | HtmlCodeChanged String
    | FormatRequested
    | FormatCompleted String
    | CollapseHtml
    | EditorsResized Float
    | ExampleSelected Example
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
    | CompileFinished (List Error)
    | WorkbenchResized Float
    | ErrorsPaneSelected ErrorsPane
    | SuccessPaneSelected SuccessPane
    | IframeReloadClicked
    | ClearLogsClicked
    | LogReceived Log
    | LogSearchChanged String
      -- Exception Stuff
    | ExceptionReceived Exception
      -- Global stuff
    | RouteChanged Route.Route
    | RevisionLoaded (Entity Revision.Id Revision)
    | AnimationFinished
    | OnlineStatusChanged Bool
    | NoOp


update : Msg -> Model -> ( Model, Outbound Msg )
update msg ({ user } as model) =
    case msg of
        ExceptionReceived exception ->
            ( { model | exceptions = exception :: model.exceptions }
            , Outbound.none
            )

        ErrorsPaneSelected pane ->
            case model.workbench of
                FinishedWithErrors state ->
                    ( { model | workbench = FinishedWithErrors { state | pane = pane } }
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
            , Outbound.Compile model.token model.elmCode model.htmlCode model.packages
            )

        CompileFinished errors ->
            case ( model.compiling, errors, model.workbench ) of
                ( True, [], Finished state ) ->
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

                ( True, [], _ ) ->
                    ( { model
                        | compiling = False
                        , workbench =
                            Finished
                                { logs = BoundedDeque.empty 50
                                , pane = SuccessOutput
                                , logSearch = ""
                                }
                      }
                    , Outbound.none
                    )

                ( True, nonEmpty, FinishedWithErrors state ) ->
                    ( { model
                        | compiling = False
                        , workbench = FinishedWithErrors { state | errors = nonEmpty }
                      }
                    , Outbound.none
                    )

                ( True, nonEmpty, _ ) ->
                    ( { model
                        | compiling = False
                        , workbench = FinishedWithErrors { errors = nonEmpty, pane = ErrorsList }
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
            ( { model | packages = model.packages ++ [ package ] }
            , Outbound.none
            )

        PackageUninstalled package ->
            ( { model | packages = List.filter ((/=) package) model.packages }
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

        RevisionLoaded ((Entity revisionId _) as entity) ->
            case model.revision of
                Replaceable.Loading rid ->
                    if rid == revisionId then
                        ( Tuple.first <| init model.token model.user (Just entity) model.defaultPackages
                        , Outbound.none
                        )
                    else
                        ( model, Outbound.none )

                Replaceable.Replacing rid _ ->
                    if rid == revisionId then
                        ( Tuple.first <| init model.token model.user (Just entity) model.defaultPackages
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
                        Replaceable.Loaded (Entity rid r) ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Replacing newRevisionId (Entity rid r) }
                                , Outbound.GetRevision newRevisionId RevisionLoaded
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.Loading rid ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Loading newRevisionId }
                                , Outbound.GetRevision newRevisionId RevisionLoaded
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.Replacing rid entity ->
                            if newRevisionId /= rid then
                                ( { model | revision = Replaceable.Replacing newRevisionId entity }
                                , Outbound.GetRevision newRevisionId RevisionLoaded
                                )
                            else
                                ( model, Outbound.none )

                        Replaceable.NotAsked ->
                            ( { model | revision = Replaceable.Loading newRevisionId }
                            , Outbound.GetRevision newRevisionId RevisionLoaded
                            )

                Route.New ->
                    case model.revision of
                        Replaceable.NotAsked ->
                            ( model, Outbound.none )

                        _ ->
                            ( Tuple.first <| init model.token model.user Nothing model.defaultPackages
                            , Outbound.none
                            )

                Route.NotFound ->
                    case Replaceable.toMaybe model.revision of
                        Just (Entity rid _) ->
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
        , Inbound.CompileFinished model.token CompileFinished
        , if model.connected then
            Inbound.WorkspaceDetached model.token (OnlineStatusChanged False)
          else
            Inbound.WorkspaceAttached model.token (\_ -> OnlineStatusChanged True)
        ]
