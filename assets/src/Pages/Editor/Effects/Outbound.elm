port module Pages.Editor.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        )

import Data.Jwt as Jwt exposing (Jwt)
import Elm.Docs as Docs
import Elm.Error as ElmError
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Elm.Version as Version exposing (Version)
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
    | UpdateRevision Jwt String Revision (RevisionId -> Revision -> msg)
    | GetRevision RevisionId (Revision -> msg)
    | Authenticate (Maybe Jwt) (Int -> Jwt -> User -> msg)
    | AcceptTerms Jwt Int msg
    | SaveSettings Jwt Settings
    | SaveToken Jwt
    | FormatElmCode Version String (String -> msg)
    | Compile Jwt Version String (List Package)
    | ReloadIframe
    | Redirect String
    | Navigate String
    | SearchPackages String (Maybe (List Package) -> msg)
    | EnableNavigationCheck Bool
    | DownloadZip { project : Project, elm : ElmSource, html : HtmlSource }
    | OpenInNewTab String
    | Batch (List (Outbound msg))
    | Delay Float msg
    | MoveElmCursor ElmError.Position
    | AttachToWorkspace Jwt Version
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

        FormatElmCode version input callback ->
            FormatElmCode version input (callback >> f)

        SaveToken token ->
            SaveToken token

        SearchPackages query callback ->
            SearchPackages query (callback >> f)

        Compile token version elm packages ->
            Compile token version elm packages

        ReloadIframe ->
            ReloadIframe

        Redirect url ->
            Redirect url

        AttachToWorkspace token version ->
            AttachToWorkspace token version

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
