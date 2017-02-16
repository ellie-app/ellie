module Apps.Editor.Model
    exposing
        ( Model
        , PopoutState(..)
        , Flags
        , model
        , resetToNew
        , updateClientRevision
        , isOwnedProject
        , isSavedProject
        , isRevisionChanged
        , commitStagedCode
        , resetStagedCode
        , canCompile
        , canSave
        , closeSearch
        , elmCodeForCompile
        , shouldWriteIframe
        , shouldCompileElm
        , hasUnsavedWork
        , activeProjectIdUrlEncoding
        )

import Set exposing (Set)
import Window exposing (Size)
import RemoteData exposing (RemoteData(..))
import Types.ApiError as ApiError exposing (ApiError)
import Types.Session as Session exposing (Session)
import Types.Revision as Revision exposing (Revision)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Notification as Notification exposing (Notification)
import Types.Package as Package exposing (Package)
import Types.ProjectId as ProjectId exposing (ProjectId)
import Apps.Editor.Routing as Routing exposing (Route(..))


type alias Flags =
    { windowSize : Window.Size
    , online : Bool
    }


type PopoutState
    = AllClosed
    | AboutOpen
    | NotificationsOpen
    | EmbedLinkOpen


type alias Model =
    { session : RemoteData ApiError Session
    , serverRevision : RemoteData ApiError Revision
    , clientRevision : Revision
    , currentRoute : Route
    , compileResult : RemoteData ApiError (List CompileError)
    , iframeResult : RemoteData ApiError ()
    , stagedElmCode : String
    , previousElmCode : String
    , stagedHtmlCode : String
    , previousHtmlCode : String
    , firstCompileComplete : Bool
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
    , installingPackage : Maybe Package
    , removingDependencyHashes : Set String
    }


model : Flags -> Model
model flags =
    { session = NotAsked
    , serverRevision = NotAsked
    , clientRevision = Revision.empty
    , stagedElmCode = .elmCode Revision.empty
    , previousElmCode = .elmCode Revision.empty
    , stagedHtmlCode = .htmlCode Revision.empty
    , previousHtmlCode = .htmlCode Revision.empty
    , currentRoute = NotFound
    , compileResult = NotAsked
    , iframeResult = NotAsked
    , firstCompileComplete = False
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
    , installingPackage = Nothing
    , removingDependencyHashes = Set.empty
    }


resetToNew : Model -> Model
resetToNew m =
    model { windowSize = m.windowSize, online = m.isOnline }


closeSearch : Model -> Model
closeSearch model =
    { model
        | searchOpen = False
        , searchResults = []
        , searchValue = ""
    }


canCompile : Model -> Bool
canCompile model =
    let
        stagedCodeChanged =
            (model.stagedElmCode /= model.previousElmCode)
                || (model.stagedHtmlCode /= model.previousHtmlCode)
    in
        not (RemoteData.isLoading model.compileResult)
            && RemoteData.isSuccess model.session
            && RemoteData.isSuccess model.serverRevision
            && ((not model.firstCompileComplete) || stagedCodeChanged)
            && model.isOnline


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


shouldCompileElm : Model -> Bool
shouldCompileElm model =
    (not model.firstCompileComplete) || model.stagedElmCode /= model.previousElmCode


shouldWriteIframe : Model -> Bool
shouldWriteIframe model =
    model.stagedHtmlCode /= model.previousHtmlCode


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
