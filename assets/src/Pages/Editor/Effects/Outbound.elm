port module Pages.Editor.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        )

import Data.Jwt as Jwt exposing (Jwt)
import Data.Uuid as Uuid exposing (Uuid)
import Elm.Docs as Docs
import Elm.Error as ElmError
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.Settings as Settings exposing (Settings)
import Pages.Editor.Types.User as User exposing (User)


type alias ElmSource =
    String


type alias HtmlSource =
    String


type Outbound msg
    = CreateRevision Jwt Revision (RevisionId -> Revision -> msg)
    | UpdateRevision Jwt Uuid Revision (RevisionId -> Revision -> msg)
    | GetRevision RevisionId (Revision -> msg)
    | Authenticate (Maybe Jwt) (Int -> Jwt -> User -> msg)
    | AcceptTerms Jwt Int msg
    | SaveSettings Jwt Settings
    | SaveToken Jwt
    | FormatElmCode String (String -> msg)
    | Compile Jwt String (List Package)
    | ReloadIframe
    | Redirect String
    | Navigate String
    | SearchPackages String (Maybe (List Package) -> msg)
    | SwitchToDebugger
    | SwitchToProgram
    | EnableNavigationCheck Bool
    | CreateGist { title : String, project : Project, elm : ElmSource, html : HtmlSource } (Maybe String -> msg)
    | DownloadZip { project : Project, elm : ElmSource, html : HtmlSource }
    | OpenInNewTab String
    | Batch (List (Outbound msg))
    | Delay Float msg
    | MoveElmCursor ElmError.Position
    | AttachToWorkspace Jwt
    | GetDocs (List Package) (List Docs.Module -> msg)
    | None


batch : List (Outbound msg) -> Outbound msg
batch =
    Batch


none : Outbound msg
none =
    None


map : (a -> b) -> Outbound a -> Outbound b
map f outbound =
    case outbound of
        GetDocs packages callback ->
            GetDocs packages (callback >> f)

        Navigate url ->
            Navigate url

        CreateRevision token revision callback ->
            CreateRevision token revision (\rid r -> f (callback rid r))

        UpdateRevision token projectId revision callback ->
            UpdateRevision token projectId revision (\rid r -> f (callback rid r))

        MoveElmCursor location ->
            MoveElmCursor location

        DownloadZip stuff ->
            DownloadZip stuff

        OpenInNewTab url ->
            OpenInNewTab url

        CreateGist stuff callback ->
            CreateGist stuff (callback >> f)

        SaveSettings token settings ->
            SaveSettings token settings

        Delay time msg ->
            Delay time (f msg)

        EnableNavigationCheck enabled ->
            EnableNavigationCheck enabled

        GetRevision id callback ->
            GetRevision id (callback >> f)

        Authenticate maybeToken callback ->
            Authenticate maybeToken <| \a b c -> f (callback a b c)

        AcceptTerms token termsVersion msg ->
            AcceptTerms token termsVersion <| f msg

        FormatElmCode input callback ->
            FormatElmCode input (callback >> f)

        SaveToken token ->
            SaveToken token

        SearchPackages query callback ->
            SearchPackages query (callback >> f)

        Compile token elm packages ->
            Compile token elm packages

        ReloadIframe ->
            ReloadIframe

        Redirect url ->
            Redirect url

        SwitchToDebugger ->
            SwitchToDebugger

        SwitchToProgram ->
            SwitchToProgram

        AttachToWorkspace token ->
            AttachToWorkspace token

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
