module App.Update exposing (update, initialize, Msg(..))

import Task
import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Components.Sidebar.Update as Sidebar
import App.Model as Model exposing (Model)
import Shared.Api as Api exposing (Session, Error, CompileError)
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
    | NoOp


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ Api.createSession
            |> Api.send InitCompleted
        , Window.size
            |> Task.perform WindowSizeChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        NoOp ->
            ( model, Cmd.none )
