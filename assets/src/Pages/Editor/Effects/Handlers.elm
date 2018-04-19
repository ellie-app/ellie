module Pages.Editor.Effects.Handlers
    exposing
        ( GqlError
        , acceptTerms
        , attachToWorkspace
        , authenticate
        , compile
        , createGist
        , createRevision
        , formatCode
        , getDocs
        , getRevision
        , searchPackages
        , setupSocket
        , updateRevision
        , updateSettings
        )

import Data.Jwt as Jwt exposing (Jwt)
import Data.Uuid as Uuid exposing (Uuid)
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
import Elm.Compiler as Compiler
import Elm.Docs as Docs
import Elm.Error as ElmError
import Elm.Name as Name
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Elm.Version as Version exposing (Version)
import Graphqelm.Field as Field
import Graphqelm.Http as GraphqlHttp
import Graphqelm.OptionalArgument as OptionalArgument
import Graphqelm.SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Http
import HttpBuilder exposing (post, withExpectJson, withJsonBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Network.Absinthe.Subscription as Subscription
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Editor.Types.Settings as Settings exposing (Settings)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate(..))
import Task exposing (Task)


type alias GqlError =
    GraphqlHttp.Error ()


getRevision : RevisionId -> Cmd (Result GqlError Revision)
getRevision revisionId =
    let
        query =
            ApiQuery.selection identity
                |> with (ApiQuery.revision arguments revisionQuery)

        arguments =
            { projectId = ApiScalar.Uuid <| Uuid.toString revisionId.projectId
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
    query
        |> GraphqlHttp.queryRequestWithHttpGet "/api" GraphqlHttp.AlwaysGet
        |> GraphqlHttp.withQueryParams [ ( "cache", "permanent" ) ]
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


searchPackages : String -> Cmd (Result GqlError (List Package))
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
    query
        |> GraphqlHttp.queryRequestWithHttpGet "/api" GraphqlHttp.AlwaysGet
        |> GraphqlHttp.withQueryParams [ ( "cache", "temporary" ) ]
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


acceptTerms : Jwt -> Int -> Cmd (Result GqlError ())
acceptTerms token terms =
    let
        mutation =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.acceptTerms { terms = terms })
    in
    mutation
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


authenticate : Maybe Jwt -> Cmd (Result GqlError ( Int, Jwt, User ))
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
    mutation
        |> GraphqlHttp.mutationRequest "/api"
        |> ApiHelpers.withMaybe Jwt.withTokenHeader maybeToken
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


formatCode : Version -> String -> Cmd (Result GqlError String)
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
    mutation
        |> GraphqlHttp.mutationRequest "/api"
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


compile : Jwt -> Version -> String -> List Package -> Cmd (Result GqlError ())
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
    mutation
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


setupSocket : Jwt -> Subscription.State WorkspaceUpdate
setupSocket token =
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
                    |> with (ApiCompileCompleted.error ElmError.selection)
                    |> ApiWorkspaceUpdate.onCompileCompleted
                ]
    in
    Subscription.init
        ("ws://localhost:4000/api/sockets/websocket?vsn=2.0.0&token=" ++ Jwt.toString token)
        Debug.log
        selection


attachToWorkspace : Jwt -> Version -> Cmd (Result GqlError ())
attachToWorkspace token version =
    let
        selection =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.attachToWorkspace arguments)

        arguments =
            { elmVersion = ApiScalar.Version <| Version.toString version }
    in
    selection
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


createGist : String -> String -> String -> Project -> Cmd (Result Http.Error String)
createGist title elm html project =
    let
        body =
            Encode.object
                [ ( "description", Encode.string title )
                , ( "public", Encode.bool False )
                , ( "files"
                  , Encode.object
                        [ ( "elm.json"
                          , Encode.object [ ( "content", Encode.string <| Encode.encode 2 (Project.encoder project) ) ]
                          )
                        , ( "Main.elm"
                          , Encode.object [ ( "content", Encode.string elm ) ]
                          )
                        , ( "index.html"
                          , Encode.object [ ( "content", Encode.string html ) ]
                          )
                        ]
                  )
                ]
    in
    post "https://api.github.com/gists"
        |> withJsonBody body
        |> withExpectJson (Decode.field "html_url" Decode.string)
        |> HttpBuilder.send identity


updateSettings : Jwt -> Settings -> Task GqlError ()
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
    selection
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.toTask
        |> Task.mapError GraphqlHttp.ignoreParsedErrorData


createRevision : Jwt -> Revision -> Cmd (Result GqlError RevisionId)
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
                |> with (ApiHelpers.uuidField ApiRevision.projectId)
                |> with ApiRevision.revisionNumber
    in
    selection
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


updateRevision : Jwt -> Uuid -> Revision -> Cmd (Result GqlError RevisionId)
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
            , projectId = ApiScalar.Uuid <| Uuid.toString projectId
            }

        revisionSelection =
            ApiRevision.selection RevisionId
                |> with (ApiHelpers.uuidField ApiRevision.projectId)
                |> with ApiRevision.revisionNumber
    in
    selection
        |> GraphqlHttp.mutationRequest "/api"
        |> Jwt.withTokenHeader token
        |> GraphqlHttp.send (Result.mapError GraphqlHttp.ignoreParsedErrorData)


getDocs : List Package -> Cmd (List Docs.Module)
getDocs packages =
    let
        selection =
            ApiQuery.selection List.concat
                |> with (ApiQuery.packages { packages = List.map makeArgs packages } packageSelection)

        makeArgs package =
            { name = ApiScalar.Name <| Name.toString package.name
            , version = ApiScalar.Version <| Version.toString package.version
            }

        packageSelection =
            ApiPackage.selection identity
                |> with (ApiPackage.docs Docs.selection)
    in
    selection
        |> GraphqlHttp.queryRequestWithHttpGet "/api" GraphqlHttp.AlwaysGet
        |> GraphqlHttp.withQueryParams [ ( "cache", "permanent" ) ]
        |> GraphqlHttp.send (Result.withDefault [])
