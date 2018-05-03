module Pages.Embed.Effects
    exposing
        ( embedUpdates
        , getRevision
        , goToPosition
        , runEmbed
        )

import Effect.Command as Command exposing (Command)
import Effect.Subscription as Subscription exposing (Subscription)
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
import Ellie.Constants as Constants
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Graphqelm.Http
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Json.Encode as Encode exposing (Value)
import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


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
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok query
        , onError = Err
        , debounce = Nothing
        , cache = Command.Permanent
        }


runEmbed : RevisionId -> Command (Result (Graphqelm.Http.Error ()) (Maybe (Maybe Error)))
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
    Command.GraphqlMutation
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok selection
        , onError = Err
        , debounce = Nothing
        }


embedUpdates : RevisionId -> Subscription EmbedUpdate
embedUpdates revisionId =
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
    Subscription.AbsintheSubscription
        (Constants.socketOrigin ++ "/api/sockets/websocket?vsn=2.0.0")
        selection
        (\connected ->
            if connected then
                EmbedUpdate.Connected
            else
                EmbedUpdate.Disconnected
        )


goToPosition : Error.Position -> Command msg
goToPosition position =
    Command.PortSend "GoToPosition" <|
        Encode.list
            [ Encode.int position.line
            , Encode.int position.column
            ]
