module Apps.Editor.Model
    exposing
        ( Model
        , PopoutState(..)
        , EditorCollapseState(..)
        , Flags
        , model
        , resetToNew
        , updateClientRevision
        , isOwnedProject
        , isSavedProject
        , isRevisionChanged
        , commitStagedCode
        , resetStagedCode
        , canSave
        , closeSearch
        , elmCodeForCompile
        , hasUnsavedWork
        , activeProjectIdUrlEncoding
        , htmlIsHidden
        , elmIsHidden
        , compileErrors
        )

import Window exposing (Size)
import RemoteData exposing (RemoteData(..))
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Notification as Notification exposing (Notification)
import Types.Package as Package exposing (Package)
import Types.ProjectId as ProjectId exposing (ProjectId)
import Types.CompileStage as CompileStage exposing (CompileStage)
import Apps.Editor.Routing as Routing exposing (Route(..))


type alias Flags =
    { windowSize : Window.Size
    , online : Bool
    , vimMode : Bool
    }


type PopoutState
    = AllClosed
    | AboutOpen
    | NotificationsOpen
    | EmbedLinkOpen


type EditorCollapseState
    = BothOpen
    | JustHtmlOpen
    | JustElmOpen


type alias Model =
    { currentRoute : Route
    , serverRevision : RemoteData ApiError Revision
    , clientRevision : Revision
    , stagedElmCode : String
    , stagedHtmlCode : String
    , previousElmCode : String
    , previousHtmlCode : String
    , compileStage : CompileStage
    , saveState : RemoteData ApiError ()
    , isOnline : Bool
    , notifications : List Notification
    , unseenNotificationsCount : Int
    , popoutState : PopoutState
    , resultSplit : Float
    , resultDragging : Bool
    , editorSplit : Float
    , editorDragging : Bool
    , windowSize : Size
    , searchOpen : Bool
    , searchValue : String
    , searchResults : List Package
    , vimMode : Bool
    , packagesChanged : Bool
    , editorsCollapse : EditorCollapseState
    , resultsCollapse : Bool
    , creatingGist : Bool
    }


model : Flags -> Model
model flags =
    { serverRevision = NotAsked
    , clientRevision = Revision.empty
    , stagedElmCode = .elmCode Revision.empty
    , previousElmCode = .elmCode Revision.empty
    , stagedHtmlCode = .htmlCode Revision.empty
    , previousHtmlCode = .htmlCode Revision.empty
    , compileStage = CompileStage.Initial
    , currentRoute = NotFound
    , saveState = NotAsked
    , isOnline = flags.online
    , notifications = []
    , unseenNotificationsCount = 0
    , popoutState = AllClosed
    , resultSplit = 0.5
    , resultDragging = False
    , editorSplit = 0.5
    , editorDragging = False
    , windowSize = flags.windowSize
    , searchOpen = False
    , searchValue = ""
    , searchResults = []
    , vimMode = flags.vimMode
    , packagesChanged = False
    , editorsCollapse = BothOpen
    , resultsCollapse = False
    , creatingGist = False
    }


resetToNew : Model -> Model
resetToNew m =
    model { windowSize = m.windowSize, online = m.isOnline, vimMode = m.vimMode }


closeSearch : Model -> Model
closeSearch model =
    { model
        | searchOpen = False
        , searchResults = []
        , searchValue = ""
    }


canSave : Model -> Bool
canSave model =
    let
        stagedCodeChanged =
            (model.stagedElmCode /= model.clientRevision.elmCode)
                || (model.stagedHtmlCode /= model.clientRevision.htmlCode)
    in
        (stagedCodeChanged || isRevisionChanged model || not (isSavedProject model))
            && not (RemoteData.isLoading model.saveState)
            && model.isOnline


isRevisionChanged : Model -> Bool
isRevisionChanged model =
    model.serverRevision
        |> RemoteData.map ((/=) model.clientRevision)
        |> RemoteData.withDefault False


isSavedProject : Model -> Bool
isSavedProject model =
    model.serverRevision
        |> RemoteData.toMaybe
        |> Maybe.andThen .projectId
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


isOwnedProject : Model -> Bool
isOwnedProject model =
    model.serverRevision
        |> RemoteData.toMaybe
        |> Maybe.map .owned
        |> Maybe.withDefault False


updateClientRevision : (Revision -> Revision) -> Model -> Model
updateClientRevision updater model =
    { model | clientRevision = updater model.clientRevision }


commitStagedCode : Model -> Model
commitStagedCode model =
    model
        |> updateClientRevision (\r -> { r | htmlCode = model.stagedHtmlCode, elmCode = model.stagedElmCode })


resetStagedCode : Model -> Model
resetStagedCode model =
    { model
        | stagedElmCode = model.clientRevision.elmCode
        , stagedHtmlCode = model.clientRevision.htmlCode
    }


elmCodeForCompile : Model -> Maybe String
elmCodeForCompile model =
    if model.stagedElmCode == model.clientRevision.elmCode then
        Nothing
    else
        Just model.stagedElmCode


hasUnsavedWork : Model -> Bool
hasUnsavedWork model =
    case model.serverRevision of
        Success revision ->
            (model.stagedElmCode /= revision.elmCode)
                || (model.stagedHtmlCode /= revision.htmlCode)
                || (model.clientRevision /= revision)

        _ ->
            False


activeProjectIdUrlEncoding : Model -> ProjectId.EncodingVersion
activeProjectIdUrlEncoding model =
    case model.currentRoute of
        SpecificRevision projectId _ ->
            ProjectId.encodingVersion projectId

        _ ->
            ProjectId.latestVersion


htmlIsHidden : Model -> Bool
htmlIsHidden model =
    model.editorsCollapse == JustElmOpen


elmIsHidden : Model -> Bool
elmIsHidden model =
    model.editorsCollapse == JustHtmlOpen


compileErrors : Model -> List CompileError
compileErrors model =
    case model.compileStage of
        CompileStage.FinishedWithErrors errors ->
            errors

        _ ->
            []
