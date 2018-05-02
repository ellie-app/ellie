module Pages.Editor.Effects exposing (..)

import Data.Jwt as Jwt exposing (Jwt)
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Ellie.Api.Enum.Theme as ApiTheme
import Ellie.Api.Helpers as ApiHelpers
import Ellie.Api.Mutation as ApiMutation
import Ellie.Api.Object.CompileCompleted as ApiCompileCompleted
import Ellie.Api.Object.Package as ApiPackage
import Ellie.Api.Object.Revision as ApiRevision
import Ellie.Api.Object.Settings as ApiSettings
import Ellie.Api.Object.User as ApiUser
import Ellie.Api.Object.UserAuth as ApiUserAuth
import Ellie.Api.Object.WorkspaceAttached as ApiWorkspaceAttached
import Ellie.Api.Query as ApiQuery
import Ellie.Api.Scalar as ApiScalar
import Ellie.Api.Subscription as ApiSubscription
import Ellie.Api.Union.WorkspaceUpdate as ApiWorkspaceUpdate
import Ellie.Constants as Constants
import Elm.Docs as Docs
import Elm.Error as Error
import Elm.Name as Name
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Elm.Version as Version exposing (Version)
import Graphqelm.Field as Field
import Graphqelm.Http
import Graphqelm.OptionalArgument as OptionalArgument
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.Settings as Settings exposing (Settings)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate(..))


getRevision : RevisionId -> Command (Result (Graphqelm.Http.Error ()) Revision)
getRevision revisionId =
    let
        query =
            ApiQuery.selection identity
                |> with (ApiQuery.revision arguments revisionQuery)

        arguments =
            { projectId = ApiScalar.ProjectId revisionId.projectId
            , revisionNumber = revisionId.revisionNumber
            }

        revisionQuery =
            ApiRevision.selection Revision
                |> with ApiRevision.htmlCode
                |> with ApiRevision.elmCode
                |> with (ApiRevision.packages Package.selection)
                |> with (ApiHelpers.defaultField "" ApiRevision.title)
                |> with (ApiHelpers.versionField ApiRevision.elmVersion)
                |> with (ApiRevision.user userQuery)

        userQuery =
            ApiUser.selection identity
                |> with (ApiHelpers.uuidField ApiUser.id)
    in
    Command.GraphqlQuery
        "/api"
        Nothing
        (SelectionSet.map Ok query)
        Err


searchPackages : String -> Command (Result (Graphqelm.Http.Error ()) (List Package))
searchPackages queryString =
    let
        query =
            ApiQuery.selection identity
                |> with (ApiQuery.packageSearch arguments packageQuery)

        arguments =
            { query = queryString }

        packageQuery =
            ApiPackage.selection Package
                |> with (ApiHelpers.nameField ApiPackage.name)
                |> with (ApiHelpers.versionField ApiPackage.version)
    in
    Command.GraphqlQuery "/api" Nothing (SelectionSet.map Ok query) Err


acceptTerms : Jwt -> Int -> Command (Result (Graphqelm.Http.Error ()) ())
acceptTerms token terms =
    let
        mutation =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.acceptTerms { terms = terms })
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok mutation) Err


authenticate : Maybe Jwt -> Command (Result (Graphqelm.Http.Error ()) ( Int, Jwt, User ))
authenticate maybeToken =
    let
        mutation =
            ApiMutation.selection identity
                |> with (ApiMutation.authenticate authSelection)

        authSelection =
            ApiUserAuth.selection (,,)
                |> with ApiUserAuth.termsVersion
                |> with (Field.map Jwt.fromString ApiUserAuth.token)
                |> with (ApiUserAuth.user userSelection)

        userSelection =
            ApiUser.selection User
                |> with (ApiHelpers.uuidField ApiUser.id)
                |> with (ApiUser.settings settingsSelection)
                |> with ApiUser.termsVersion

        settingsSelection =
            ApiSettings.selection Settings
                |> with ApiSettings.fontSize
                |> with ApiSettings.fontFamily
                |> with (Field.map makeTheme ApiSettings.theme)
                |> with ApiSettings.vimMode

        makeTheme theme =
            case theme of
                ApiTheme.Dark ->
                    Settings.Dark

                ApiTheme.Light ->
                    Settings.Light
    in
    Command.GraphqlMutation "/api" maybeToken (SelectionSet.map Ok mutation) Err


formatCode : Version -> String -> Command (Result (Graphqelm.Http.Error ()) String)
formatCode version code =
    let
        mutation =
            ApiMutation.selection identity
                |> with (ApiMutation.formatCode arguments)

        arguments =
            { code = code
            , elmVersion = ApiScalar.Version <| Version.toString version
            }
    in
    Command.GraphqlMutation "/api" Nothing (SelectionSet.map Ok mutation) Err


compile : Jwt -> Version -> String -> List Package -> Command (Result (Graphqelm.Http.Error ()) ())
compile token elmVersion elmCode packages =
    let
        mutation =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.compile arguments)

        arguments =
            { elmCode = elmCode
            , packages = List.map makePackageInput packages
            , elmVersion = ApiScalar.Version <| Version.toString elmVersion
            }

        makePackageInput package =
            { name = ApiScalar.Name <| Name.toString package.name
            , version = ApiScalar.Version <| Version.toString package.version
            }
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok mutation) Err


workspaceUpdates : Jwt -> Subscription WorkspaceUpdate
workspaceUpdates token =
    let
        selection =
            ApiSubscription.selection identity
                |> with (ApiSubscription.workspace workspaceUpdateSelection)

        workspaceUpdateSelection =
            ApiWorkspaceUpdate.selection (Maybe.withDefault Disconnected)
                [ ApiWorkspaceAttached.selection Attached
                    |> with (ApiWorkspaceAttached.packages Package.selection)
                    |> ApiWorkspaceUpdate.onWorkspaceAttached
                , ApiCompileCompleted.selection CompileCompleted
                    |> with (ApiCompileCompleted.error Error.selection)
                    |> ApiWorkspaceUpdate.onCompileCompleted
                ]
    in
    Subscription.AbsintheSubscription
        (Constants.socketOrigin ++ "/api/sockets/websocket?vsn=2.0.0&token=" ++ Jwt.toString token)
        selection
        (\connected ->
            if connected then
                Connected
            else
                Disconnected
        )


attachToWorkspace : Jwt -> Version -> Command (Result (Graphqelm.Http.Error ()) ())
attachToWorkspace token version =
    let
        selection =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.attachToWorkspace arguments)

        arguments =
            { elmVersion = ApiScalar.Version <| Version.toString version }
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok selection) Err


updateSettings : Jwt -> Settings -> Command (Result (Graphqelm.Http.Error ()) ())
updateSettings token settings =
    let
        selection =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.updateSettings (\_ -> arguments))

        arguments =
            { fontSize = OptionalArgument.Present settings.fontSize
            , fontFamily = OptionalArgument.Present settings.fontFamily
            , vimMode = OptionalArgument.Present settings.vimMode
            , theme =
                OptionalArgument.Present <|
                    case settings.theme of
                        Settings.Dark ->
                            ApiTheme.Dark

                        Settings.Light ->
                            ApiTheme.Light
            }
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok selection) Err


createRevision : Jwt -> Revision -> Command (Result (Graphqelm.Http.Error ()) RevisionId)
createRevision token revision =
    let
        selection =
            ApiMutation.selection identity
                |> with (ApiMutation.createRevision arguments revisionSelection)

        arguments =
            { inputs =
                { elmCode = revision.elmCode
                , htmlCode = revision.htmlCode
                , packages = List.map Package.toInputObject revision.packages
                , title =
                    case revision.title of
                        "" ->
                            OptionalArgument.Absent

                        title ->
                            OptionalArgument.Present title
                }
            }

        revisionSelection =
            ApiRevision.selection RevisionId
                |> with (ApiHelpers.projectIdField ApiRevision.projectId)
                |> with ApiRevision.revisionNumber
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok selection) Err


updateRevision : Jwt -> String -> Revision -> Command (Result (Graphqelm.Http.Error ()) RevisionId)
updateRevision token projectId revision =
    let
        selection =
            ApiMutation.selection identity
                |> with (ApiMutation.updateRevision arguments revisionSelection)

        arguments =
            { inputs =
                { elmCode = revision.elmCode
                , htmlCode = revision.htmlCode
                , packages = List.map Package.toInputObject revision.packages
                , title =
                    case revision.title of
                        "" ->
                            OptionalArgument.Absent

                        title ->
                            OptionalArgument.Present title
                }
            , projectId = ApiScalar.ProjectId <| projectId
            }

        revisionSelection =
            ApiRevision.selection RevisionId
                |> with (ApiHelpers.projectIdField ApiRevision.projectId)
                |> with ApiRevision.revisionNumber
    in
    Command.GraphqlMutation "/api" (Just token) (SelectionSet.map Ok selection) Err


getDocs : List Package -> Command (List Docs.Module)
getDocs packages =
    let
        selection =
            ApiQuery.selection List.concat
                |> with (ApiQuery.packages { packages = List.map makeArgs packages } docsSelection)

        makeArgs package =
            { name = ApiScalar.Name <| Name.toString package.name
            , version = ApiScalar.Version <| Version.toString package.version
            }

        packageSelection =
            ApiPackage.selection Package
                |> with (ApiHelpers.nameField ApiPackage.name)
                |> with (ApiHelpers.versionField ApiPackage.version)

        docsSelection =
            packageSelection
                |> SelectionSet.map (\p d -> List.map ((|>) p) d)
                |> with (ApiPackage.docs Docs.selection)
    in
    Command.GraphqlQuery "/api" Nothing (SelectionSet.map identity selection) (\_ -> [])


moveElmCursor : Error.Position -> Command msg
moveElmCursor position =
    Command.PortSend "MoveElmCursor" <|
        Encode.list
            [ Encode.int position.line
            , Encode.int position.column
            ]


downloadZip : String -> String -> Project -> Command msg
downloadZip elm html project =
    Command.PortSend "DownloadZip" <|
        Encode.list
            [ Encode.string <| Encode.encode 2 (Project.encoder project)
            , Encode.string elm
            , Encode.string html
            ]


openInNewTab : String -> Command msg
openInNewTab url =
    Command.PortSend "OpenInNewTab" <|
        Encode.list
            [ Encode.string url ]


delay : Int -> Command ()
delay millis =
    Command.Delay millis ()


enableNavigationCheck : Bool -> Command msg
enableNavigationCheck enabled =
    Command.PortSend "EnableNavigationCheck" <|
        Encode.list
            [ Encode.bool enabled ]


saveToken : Jwt -> Command msg
saveToken token =
    Command.PortSend "SaveToken" <|
        Encode.list
            [ Jwt.encoder token ]


navigate : String -> Command msg
navigate url =
    Command.NewUrl url


redirect : String -> Command msg
redirect url =
    Command.Redirect url


escapePressed : Subscription ()
escapePressed =
    Subscription.KeyPress 27 ()


reloadOutput : Command msg
reloadOutput =
    Command.ReloadOutput


networkStatus : Subscription Bool
networkStatus =
    Subscription.PortReceive "NetworkStatus" <|
        \value ->
            case Decode.decodeValue Decode.bool value of
                Ok result ->
                    result

                Err _ ->
                    False
