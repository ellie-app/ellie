module Pages.Editor.Effects exposing (attachToWorkspace, authenticate, compile, createRevision, delay, downloadZip, escapePressed, formatCode, getDocs, getRevision, keyCombos, moveElmCursor, navigate, networkStatus, openInNewTab, redirect, reloadOutput, saveToken, searchPackages, updateRecoveryRevision, updateUser, workspaceUpdates)

import Data.Jwt as Jwt exposing (Jwt)
import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
import Ellie.Api.Helpers as ApiHelpers
import Ellie.Api.Mutation as ApiMutation
import Ellie.Api.Object.CompileCompleted as ApiCompileCompleted
import Ellie.Api.Object.ElmPackage as ApiPackage
import Ellie.Api.Object.Revision as ApiRevision
import Ellie.Api.Object.WorkspaceAttached as ApiWorkspaceAttached
import Ellie.Api.Object.WorkspaceError as ApiWorkspaceError
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
import Extra.Json.Encode as Encode
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Pages.Editor.Types.EditorAction as EditorAction exposing (EditorAction)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.User as User exposing (User)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate(..))


getRevision : Revision.Id -> Command (Result (Graphql.Http.Error ()) Revision)
getRevision revisionId =
    let
        query =
            ApiQuery.selection identity
                |> with (ApiQuery.revision arguments revisionQuery)

        arguments =
            { id = ApiScalar.PrettyId revisionId }

        revisionQuery =
            ApiRevision.selection Revision
                |> with ApiRevision.htmlCode
                |> with ApiRevision.elmCode
                |> with (ApiRevision.packages Package.selection)
                |> with (ApiHelpers.defaultField "" ApiRevision.title)
                |> with (ApiHelpers.versionField ApiRevision.elmVersion)
    in
    Command.GraphqlQuery
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok query
        , onError = Err
        , debounce = Nothing
        , cache = Command.Permanent
        }


searchPackages : String -> Command (Result (Graphql.Http.Error ()) (List Package))
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
    Command.GraphqlQuery
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok query
        , onError = Err
        , debounce = Just "package-search"
        , cache = Command.Temporary
        }


formatCode : Jwt -> Version -> String -> Command (Result (Graphql.Http.Error ()) String)
formatCode token version code =
    let
        mutation =
            ApiMutation.selection identity
                |> with (ApiMutation.formatCode arguments)

        arguments =
            { code = code
            , elmVersion = ApiScalar.ElmVersion <| Version.toString version
            }
    in
    Command.GraphqlMutation
        { url = "/api"
        , token = Just token
        , selection = SelectionSet.map Ok mutation
        , onError = Err
        , debounce = Just "format-code"
        }


compile : Jwt -> Version -> String -> List Package -> Command (Result (Graphql.Http.Error ()) ())
compile token elmVersion elmCode packages =
    let
        mutation =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.compile arguments)

        arguments =
            { elmCode = elmCode
            , packages = List.map makePackageInput packages
            , elmVersion = ApiScalar.ElmVersion <| Version.toString elmVersion
            }

        makePackageInput package =
            { name = ApiScalar.ElmName <| Name.toString package.name
            , version = ApiScalar.ElmVersion <| Version.toString package.version
            }
    in
    Command.GraphqlMutation
        { url = "/api"
        , token = Just token
        , selection = SelectionSet.map Ok mutation
        , onError = Err
        , debounce = Nothing
        }


authenticate : Command (Result (Graphql.Http.Error ()) Jwt)
authenticate =
    Command.GraphqlMutation
        { url = "/api"
        , token = Nothing
        , onError = Err
        , debounce = Nothing
        , selection =
            ApiMutation.selection (Jwt.fromString >> Ok)
                |> with ApiMutation.authenticate
        }


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
                , ApiWorkspaceError.selection (\_ -> Disconnected)
                    |> with ApiWorkspaceError.message
                    |> ApiWorkspaceUpdate.onWorkspaceError
                ]
    in
    Subscription.AbsintheSubscription
        { url = Constants.socketOrigin ++ "/api/sockets", token = Just (Jwt.toString token) }
        selection
        (\connected ->
            if connected then
                Connected

            else
                Disconnected
        )


attachToWorkspace : Jwt -> Version -> Command (Result (Graphql.Http.Error ()) ())
attachToWorkspace token version =
    let
        selection =
            ApiMutation.selection (\_ -> ())
                |> with (ApiMutation.attachToWorkspace arguments)

        arguments =
            { elmVersion = ApiScalar.ElmVersion <| Version.toString version }
    in
    Command.GraphqlMutation
        { url = "/api"
        , token = Just token
        , selection = SelectionSet.map Ok selection
        , onError = Err
        , debounce = Nothing
        }


updateUser : User -> Command a
updateUser user =
    Command.PortSend
        { channel = "UpdateUser"
        , debounce = Nothing
        , data = Encode.list User.localStorageEncoder [ user ]
        }


createRevision : Jwt -> Int -> Revision -> Command (Result (Graphql.Http.Error ()) Revision.Id)
createRevision token termsVersion revision =
    let
        selection =
            ApiMutation.selection identity
                |> with (ApiMutation.createRevision arguments revisionSelection)

        arguments =
            { inputs =
                { elmCode = revision.elmCode
                , htmlCode = revision.htmlCode
                , packages = List.map Package.toInputObject revision.packages
                , termsVersion = termsVersion
                , title =
                    case revision.title of
                        "" ->
                            OptionalArgument.Absent

                        title ->
                            OptionalArgument.Present title
                }
            }

        revisionSelection =
            ApiRevision.selection identity
                |> with (ApiHelpers.projectIdField ApiRevision.id)
    in
    Command.GraphqlMutation
        { url = "/api"
        , token = Just token
        , selection = SelectionSet.map Ok selection
        , onError = Err
        , debounce = Nothing
        }


getDocs : List Package -> Command (List Docs.Module)
getDocs packages =
    let
        selection =
            ApiQuery.selection List.concat
                |> with (ApiQuery.packages { packages = List.map makeArgs packages } docsSelection)

        makeArgs package =
            { name = ApiScalar.ElmName <| Name.toString package.name
            , version = ApiScalar.ElmVersion <| Version.toString package.version
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
    Command.GraphqlQuery
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map identity selection
        , onError = always []
        , debounce = Nothing
        , cache = Command.Permanent
        }


moveElmCursor : Error.Position -> Command msg
moveElmCursor position =
    Command.PortSend
        { channel = "MoveElmCursor"
        , debounce = Nothing
        , data =
            Encode.list Encode.int
                [ position.line
                , position.column
                ]
        }


downloadZip : String -> String -> Project -> Command msg
downloadZip elm html project =
    Command.PortSend
        { channel = "DownloadZip"
        , debounce = Nothing
        , data =
            Encode.list identity
                [ Encode.string <| Encode.encode 2 (Project.encoder project)
                , Encode.string elm
                , Encode.string html
                ]
        }


openInNewTab : String -> Command msg
openInNewTab url =
    Command.PortSend
        { channel = "OpenInNewTab"
        , debounce = Nothing
        , data = Encode.list Encode.string [ url ]
        }


delay : Int -> Command ()
delay millis =
    Command.Delay millis ()


saveToken : Jwt -> Command msg
saveToken token =
    Command.PortSend
        { channel = "SaveToken"
        , debounce = Nothing
        , data = Encode.list Jwt.encoder [ token ]
        }


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


keyCombos : Subscription EditorAction
keyCombos =
    Subscription.batch
        [ Subscription.KeyCombo { meta = True, shift = True, key = "Enter" } EditorAction.Recompile
        , Subscription.KeyCombo { meta = True, shift = True, key = "p" } EditorAction.OpenPackages
        , Subscription.KeyCombo { meta = True, shift = True, key = "d" } EditorAction.OpenDebugger
        , Subscription.KeyCombo { meta = True, shift = True, key = "o" } EditorAction.OpenOutput
        , Subscription.KeyCombo { meta = True, shift = False, key = "," } EditorAction.OpenSettings
        , Subscription.KeyCombo { meta = True, shift = True, key = "r" } EditorAction.ReloadOutput
        , Subscription.KeyCombo { meta = True, shift = True, key = "l" } EditorAction.OpenLogs
        , Subscription.KeyCombo { meta = True, shift = False, key = "s" } EditorAction.Save
        ]


updateRecoveryRevision : Maybe Revision -> Command msg
updateRecoveryRevision revision =
    Command.PortSend
        { channel = "UpdateRecoveryRevision"
        , data = Encode.list (Encode.maybeNull Revision.localStorageEncoder) [ revision ]
        , debounce = Just "UpdateRecoveryRevision"
        }
