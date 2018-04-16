module Pages.Embed.Effects.Handlers
    exposing
        ( GetRevisionError(..)
        , GqlError
        , getRevision
        )

import Data.Uuid as Uuid exposing (Uuid)
import Ellie.Api.Helpers as ApiHelpers
import Ellie.Api.Object.Revision as ApiRevision
import Ellie.Api.Object.User as ApiUser
import Ellie.Api.Query as ApiQuery
import Ellie.Api.Scalar as ApiScalar
import Elm.Package as Package exposing (Package)
import Graphqelm.Http as GraphqlHttp
import Graphqelm.SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)


type alias GqlError =
    GraphqlHttp.Error ()


type GetRevisionError
    = GetRevisionError


getRevision : RevisionId -> Cmd (Result GetRevisionError Revision)
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
        |> GraphqlHttp.queryRequest "/api"
        |> GraphqlHttp.send (Result.mapError (\_ -> GetRevisionError))



-- setupSocket : Jwt -> Subscription.State WorkspaceUpdate
-- setupSocket token =
--     let
--         selection =
--             ApiSubscription.selection identity
--                 |> with (ApiSubscription.workspace workspaceUpdateSelection)
--         workspaceUpdateSelection =
--             ApiWorkspaceUpdate.selection (Maybe.withDefault Disconnected)
--                 [ ApiWorkspaceAttached.selection Attached
--                     |> with (ApiWorkspaceAttached.packages Package.selection)
--                     |> ApiWorkspaceUpdate.onWorkspaceAttached
--                 , ApiCompileCompleted.selection CompileCompleted
--                     |> with (ApiCompileCompleted.error ElmError.selection)
--                     |> ApiWorkspaceUpdate.onCompileCompleted
--                 ]
--     in
--     Subscription.init
--         ("ws://localhost:4000/api/sockets/websocket?vsn=2.0.0&token=" ++ Jwt.toString token)
--         Debug.log
--         selection
