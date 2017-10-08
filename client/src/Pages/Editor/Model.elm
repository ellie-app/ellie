module Pages.Editor.Model
    exposing
        ( Model
        , canCompile
        , canSave
        , commitStagedCode
        , compileErrors
        , elmCodeForCompile
        , hasUnsavedWork
        , isOwnedProject
        , isRevisionChanged
        , isSavedProject
        , model
        , mustAcceptTerms
        , resetStagedCode
        , resetToNew
        , updateClientRevision
        )

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Data.Ellie.Notification as Notification exposing (Notification)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Ellie.SaveState as SaveState exposing (SaveState)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Compiler.Error as CompilerError
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Header.Model as Header
import Pages.Editor.Layout.Model as Layout
import Pages.Editor.Routing as Routing exposing (Route(..))
import Pages.Editor.Sidebar.Model as Sidebar
import RemoteData exposing (RemoteData(..))


type alias Model =
    { currentRoute : Route
    , serverRevision : RemoteData ApiError Revision
    , clientRevision : Revision
    , stagedElmCode : String
    , stagedHtmlCode : String
    , previousElmCode : String
    , previousHtmlCode : String
    , compileStage : CompileStage
    , saveState : SaveState
    , isOnline : Bool
    , notifications : List Notification
    , vimMode : Bool
    , packagesChanged : Bool
    , creatingGist : Bool
    , keyCombo : KeyCombo
    , latestTermsVersion : TermsVersion
    , acceptedTermsVersion : Maybe TermsVersion
    , sidebar : Sidebar.Model
    , layout : Layout.Model
    , header : Header.Model
    , termsShown : Bool
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
    , saveState = SaveState.Ready
    , isOnline = flags.online
    , notifications = []
    , vimMode = flags.vimMode
    , packagesChanged = False
    , creatingGist = False
    , keyCombo = KeyCombo.empty
    , latestTermsVersion = flags.latestTermsVersion
    , acceptedTermsVersion = flags.acceptedTermsVersion
    , sidebar = Sidebar.model
    , layout = Layout.init flags.windowSize
    , header = Header.init
    , termsShown = False
    }


mustAcceptTerms :
    { a
        | latestTermsVersion : TermsVersion
        , acceptedTermsVersion : Maybe TermsVersion
    }
    -> Bool
mustAcceptTerms { latestTermsVersion, acceptedTermsVersion } =
    acceptedTermsVersion /= Just latestTermsVersion


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
    stagePasses && SaveState.canSave model.saveState


resetToNew : Model -> Model
resetToNew model =
    { model
        | serverRevision = NotAsked
        , clientRevision = Revision.empty
        , stagedElmCode = .elmCode Revision.empty
        , previousElmCode = .elmCode Revision.empty
        , stagedHtmlCode = .htmlCode Revision.empty
        , previousHtmlCode = .htmlCode Revision.empty
        , compileStage = CompileStage.Initial
        , saveState = SaveState.Ready
        , vimMode = model.vimMode
        , packagesChanged = False
    }


canSave : Model -> Bool
canSave model =
    let
        stagedCodeChanged =
            (model.stagedElmCode /= model.clientRevision.elmCode)
                || (model.stagedHtmlCode /= model.clientRevision.htmlCode)
    in
    (stagedCodeChanged || isRevisionChanged model || not (isSavedProject model))
        && SaveState.canSave model.saveState
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


compileErrors : Model -> List CompilerError.Error
compileErrors model =
    case model.compileStage of
        CompileStage.FinishedWithErrors errors ->
            errors

        _ ->
            []
