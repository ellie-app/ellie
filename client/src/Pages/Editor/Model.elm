module Pages.Editor.Model
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
        , htmlIsHidden
        , elmIsHidden
        , compileErrors
        , canCompile
        )

import Window exposing (Size)
import RemoteData exposing (RemoteData(..))
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Ellie.Notification as Notification exposing (Notification)
import Data.Elm.Compiler.Error as CompilerError
import Data.Elm.Package as Package exposing (Package)
import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Pages.Editor.Routing as Routing exposing (Route(..))


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
    , keyCombo : KeyCombo
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
    , keyCombo = KeyCombo.empty
    }


canCompile : Model -> Bool
canCompile model =
    let
        stagePasses =
            case model.compileStage of
                Initial ->
                    True

                CompileStage.Success _ ->
                    True

                FinishedWithErrors _ ->
                    True

                Failed _ ->
                    True

                _ ->
                    False
    in
        stagePasses && model.saveState /= Loading


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
        |> Maybe.andThen .id
        |> Maybe.map .projectId
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
        RemoteData.Success revision ->
            (model.stagedElmCode /= revision.elmCode)
                || (model.stagedHtmlCode /= revision.htmlCode)
                || (model.clientRevision /= revision)

        _ ->
            False


htmlIsHidden : Model -> Bool
htmlIsHidden model =
    model.editorsCollapse == JustElmOpen


elmIsHidden : Model -> Bool
elmIsHidden model =
    model.editorsCollapse == JustHtmlOpen


compileErrors : Model -> List CompilerError.Error
compileErrors model =
    case model.compileStage of
        CompileStage.FinishedWithErrors errors ->
            errors

        _ ->
            []
