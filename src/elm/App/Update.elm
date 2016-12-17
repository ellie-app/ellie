module App.Update exposing (update, initialize, onRouteChange, Msg(..))

import Task
import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Navigation
import Types.ExistingRevision as ExistingRevision exposing (ExistingRevision)
import Types.Session as Session exposing (Session)
import Types.CompileError as CompileError exposing (CompileError)
import App.Model as Model exposing (Model)
import App.Routing as Routing exposing (Route(..))
import Components.Sidebar.Update as Sidebar
import Shared.Api as Api exposing (Error)
import Shared.Constants as Constants
import Shared.Utils as Utils


-- UPDATE


type Msg
    = WindowUnloaded
    | TypedElmCode String
    | TypedHtmlCode String
    | InitCompleted (RemoteData Error Session)
    | Compile
    | CompileCompleted (RemoteData Error (List CompileError))
    | StartDraggingEditors
    | EditorSplitDrags Position
    | StartDraggingResult
    | ResultSplitDrags Position
    | StopDragging
    | WindowSizeChanged Size
    | SidebarMsg Sidebar.Msg
    | TitleChanged String
    | DescriptionChanged String
    | RouteChanged Route
    | SaveButtonClicked
    | SaveCompleted (RemoteData Error ExistingRevision)
    | LoadRevisionComplete (RemoteData Error ExistingRevision)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRevisionComplete revision ->
            case revision of
                Success r ->
                    ( { model
                        | htmlCode = r.htmlCode
                        , elmCode = r.elmCode
                        , dependencies = r.dependencies
                        , currentRevision = revision
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | currentRevision = revision }
                    , Cmd.none
                    )

        SaveButtonClicked ->
            let
                newRevision =
                    { htmlCode = model.htmlCode
                    , elmCode = model.elmCode
                    , dependencies = model.dependencies
                    }
            in
                ( { model | currentRevision = Loading }
                , Api.createProjectFromRevision newRevision
                    |> Api.send SaveCompleted
                )

        SaveCompleted currentRevision ->
            ( { model | currentRevision = currentRevision }
            , case currentRevision of
                Success revision ->
                    Navigation.modifyUrl <| Routing.construct (SpecificRevision revision.projectId revision.revisionNumber)

                _ ->
                    Cmd.none
            )

        WindowUnloaded ->
            case model.session of
                Success session ->
                    ( model
                    , Api.removeSession session
                        |> Api.send (\_ -> NoOp)
                    )

                _ ->
                    ( model, Cmd.none )

        InitCompleted sessionResult ->
            ( { model | session = sessionResult }
            , Cmd.none
            )

        TypedElmCode nextCode ->
            ( { model | elmCode = nextCode }
            , Cmd.none
            )

        TypedHtmlCode nextCode ->
            ( { model | htmlCode = nextCode }
            , Cmd.none
            )

        Compile ->
            case model.session of
                Success session ->
                    ( { model | compileResult = Loading }
                    , Api.compile session model.elmCode
                        |> Api.send CompileCompleted
                    )

                _ ->
                    ( model, Cmd.none )

        CompileCompleted result ->
            ( { model | compileResult = result }
            , Cmd.none
            )

        StartDraggingEditors ->
            ( { model | isDraggingEditorsSplit = True }
            , Cmd.none
            )

        EditorSplitDrags position ->
            let
                adjustedForHeader =
                    position.y - 60

                percentage =
                    toFloat adjustedForHeader / (toFloat model.windowSize.height - 60)

                clamped =
                    if percentage < 0.1 then
                        0.1
                    else if percentage > 0.9 then
                        0.9
                    else
                        percentage
            in
                ( { model | editorEditorSplit = clamped }
                , Cmd.none
                )

        StartDraggingResult ->
            ( { model | isDraggingResultSplit = True }
            , Cmd.none
            )

        ResultSplitDrags position ->
            let
                adjustedForSidebar =
                    position.x - Constants.sidebarWidth

                percentage =
                    toFloat adjustedForSidebar / (toFloat model.windowSize.width - Constants.sidebarWidth)

                clamped =
                    if percentage < 0.1 then
                        0.1
                    else if percentage > 0.7 then
                        0.7
                    else
                        percentage
            in
                ( { model | editorsResultSplit = clamped }
                , Cmd.none
                )

        StopDragging ->
            ( { model
                | isDraggingResultSplit = False
                , isDraggingEditorsSplit = False
              }
            , Cmd.none
            )

        WindowSizeChanged size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        TitleChanged title ->
            ( { model | title = title }
            , Cmd.none
            )

        DescriptionChanged description ->
            ( { model | description = description }
            , Cmd.none
            )

        SidebarMsg subMsg ->
            Sidebar.update subMsg model.sidebar
                |> Utils.mapCmd SidebarMsg
                |> Utils.mapModel (\s -> { model | sidebar = s })

        RouteChanged route ->
            handleRouteChanged route ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


onRouteChange : Navigation.Location -> Msg
onRouteChange =
    Routing.parse >> RouteChanged


initialize : Navigation.Location -> ( Model, Cmd Msg )
initialize location =
    ( Model.model
    , Cmd.batch
        [ Api.createSession
            |> Api.send InitCompleted
        , Window.size
            |> Task.perform WindowSizeChanged
        ]
    )
        |> handleRouteChanged (Routing.parse location)


handleRouteChanged : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRouteChanged route ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            case route of
                NotFound ->
                    ( model
                    , Navigation.modifyUrl <| Routing.construct NewProject
                    )

                SpecificRevision projectId revisionNumber ->
                    let
                        alreadyLoaded =
                            case model.currentRevision of
                                Success revision ->
                                    revision.projectId
                                        == projectId
                                        && revision.revisionNumber
                                        == revisionNumber

                                _ ->
                                    False
                    in
                        if alreadyLoaded then
                            ( model, Cmd.none )
                        else
                            ( { model | currentRevision = Loading }
                            , Api.exactRevision projectId revisionNumber
                                |> Api.send LoadRevisionComplete
                            )

                LatestRevision projectId ->
                    ( { model | currentRevision = Loading }
                    , Api.latestRevision projectId
                        |> Api.send LoadRevisionComplete
                    )

                NewProject ->
                    ( model, Cmd.none )
    in
        ( { nextModel | currentRoute = route }
        , Cmd.batch [ nextCmd, cmd ]
        )
