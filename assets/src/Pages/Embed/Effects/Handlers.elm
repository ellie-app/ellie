module Pages.Embed.Effects.Handlers
    exposing
        ( GetRevisionError(..)
        , GqlError
        , RunEmbedError(..)
        , getRevision
        , runEmbed
        , socket
        )

import Ellie.Api.Helpers as ApiHelpers
import Ellie.Api.Mutation as ApiMutation
import Ellie.Api.Object.EmbedFailed as ApiEmbedFailed
import Ellie.Api.Object.EmbedReady as ApiEmbedReady
import Ellie.Api.Object.Revision as ApiRevision
import Ellie.Api.Object.User as ApiUser
import Ellie.Api.Query as ApiQuery
import Ellie.Api.Scalar as ApiScalar
import Ellie.Api.Subscription as ApiSubscription
import Ellie.Api.Union.EmbedUpdate as ApiEmbedUpdate
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Graphqelm.Http as GraphqlHttp
import Graphqelm.SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Network.Absinthe.Subscription as Subscription
import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


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
    query
        |> GraphqlHttp.queryRequest "/api"
        |> GraphqlHttp.send (Result.mapError (\_ -> GetRevisionError))


type RunEmbedError
    = RunEmbedError


runEmbed : RevisionId -> Cmd (Result RunEmbedError (Maybe (Maybe Error)))
runEmbed revisionId =
    let
        selection =
            ApiMutation.selection identity
                |> with (ApiMutation.runEmbed arguments embedReadySelection)

        embedReadySelection =
            ApiEmbedReady.selection identity
                |> with (ApiEmbedReady.error Error.selection)

        arguments =
            { projectId = ApiScalar.ProjectId revisionId.projectId
            , revisionNumber = revisionId.revisionNumber
            }
    in
    selection
        |> GraphqlHttp.mutationRequest "/api"
        |> GraphqlHttp.send (Result.mapError (\_ -> RunEmbedError))


socket : RevisionId -> Subscription.State EmbedUpdate
socket revisionId =
    let
        selection =
            ApiSubscription.selection identity
                |> with (ApiSubscription.embed arguments embedUpdateSelection)

        arguments =
            { projectId = ApiScalar.ProjectId revisionId.projectId
            , revisionNumber = revisionId.revisionNumber
            }

        embedUpdateSelection =
            ApiEmbedUpdate.selection (Maybe.withDefault (EmbedUpdate.Failed "Missing data"))
                [ ApiEmbedReady.selection EmbedUpdate.Compiled
                    |> with (ApiEmbedReady.error Error.selection)
                    |> ApiEmbedUpdate.onEmbedReady
                , ApiEmbedFailed.selection EmbedUpdate.Failed
                    |> with ApiEmbedFailed.message
                    |> ApiEmbedUpdate.onEmbedFailed
                ]
    in
    Subscription.init
        "ws://localhost:4000/api/sockets/websocket?vsn=2.0.0"
        Debug.log
        -- Subscription.emptyLogger
        selection
