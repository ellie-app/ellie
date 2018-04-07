port module Pages.Editor.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Types.User as User exposing (User)
import Elm.Compiler.Error as Compiler
import Elm.Docs as Docs exposing (Module)
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)


type alias ElmSource =
    String


type alias HtmlSource =
    String


type Outbound msg
    = CreateRevision Jwt Revision (Result String (Entity Revision.Id Revision) -> msg)
    | UpdateRevision Jwt (Entity Revision.Id Revision) (Result String (Entity Revision.Id Revision) -> msg)
    | GetRevision Revision.Id (Entity Revision.Id Revision -> msg)
    | Authenticate (Maybe Jwt) (TermsVersion -> Jwt -> Entity User.Id User -> msg)
    | AcceptTerms Jwt TermsVersion msg
    | SaveSettings Jwt Settings
    | SaveToken Jwt
    | FormatElmCode String (String -> msg)
    | Compile Jwt String String (List Package)
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
    | MoveElmCursor Compiler.Location
    | GetDocs (List Package) (List Module -> msg)
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
            CreateRevision token revision (callback >> f)

        UpdateRevision token entity callback ->
            UpdateRevision token entity (callback >> f)

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

        Compile token elm html packages ->
            Compile token elm html packages

        ReloadIframe ->
            ReloadIframe

        Redirect url ->
            Redirect url

        SwitchToDebugger ->
            SwitchToDebugger

        SwitchToProgram ->
            SwitchToProgram

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
