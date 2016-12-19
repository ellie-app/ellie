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


withNoCmd : Model -> ( Model, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


saveRevision : Model -> ( Model, Cmd Msg )
saveRevision model =
    ( { model | currentRevision = Loading }
    , model
        |> Model.updatedRevision
        |> Maybe.andThen (Utils.filterMaybe .owned)
        |> Maybe.map Api.createRevision
        |> Maybe.withDefault (Api.createProjectFromRevision (Model.newRevision model))
        |> Api.send SaveCompleted
    )


redirectIfSuccessful : RemoteData Error ExistingRevision -> Model -> ( Model, Cmd Msg )
redirectIfSuccessful data model =
    ( model
    , case data of
        Success revision ->
            Navigation.modifyUrl <| Routing.construct (SpecificRevision revision.projectId revision.revisionNumber)

        _ ->
            Cmd.none
    )


unloadSession : Model -> ( Model, Cmd Msg )
unloadSession model =
    case model.session of
        Success session ->
            ( model
            , Api.removeSession session
                |> Api.send (\_ -> NoOp)
            )

        _ ->
            ( model, Cmd.none )


compileIfReady : Model -> ( Model, Cmd Msg )
compileIfReady model =
    case model.session of
        Success session ->
            ( model
            , Api.compile session model.elmCode
                |> Api.send CompileCompleted
            )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRevisionComplete revision ->
            model
                |> Model.acceptLoadedRevision revision
                |> withNoCmd

        SaveButtonClicked ->
            model
                |> saveRevision

        SaveCompleted currentRevision ->
            model
                |> Model.acceptSavedRevision currentRevision
                |> redirectIfSuccessful currentRevision

        WindowUnloaded ->
            model
                |> unloadSession

        InitCompleted sessionResult ->
            model
                |> Model.acceptSession sessionResult
                |> withNoCmd

        TypedElmCode nextCode ->
            model
                |> Model.updateElmCode nextCode
                |> withNoCmd

        TypedHtmlCode nextCode ->
            model
                |> Model.updateHtmlCode nextCode
                |> withNoCmd

        Compile ->
            model
                |> Model.startCompiling
                |> compileIfReady

        CompileCompleted result ->
            model
                |> Model.acceptCompileResult result
                |> withNoCmd

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
