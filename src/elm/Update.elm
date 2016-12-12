module Update exposing (update, initialize, Msg(..))

import Task
import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Api exposing (Session, Error, CompileError)
import Model exposing (Model)


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
                    position.x

                percentage =
                    toFloat adjustedForSidebar / (toFloat model.windowSize.width)

                clamped =
                    if percentage < 0.1 then
                        0.1
                    else if percentage > 0.9 then
                        0.9
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

        NoOp ->
            ( model, Cmd.none )
