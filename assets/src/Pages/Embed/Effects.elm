module Pages.Embed.Effects exposing
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
import Ellie.Api.Query as ApiQuery
import Ellie.Api.Scalar as ApiScalar
import Ellie.Api.Subscription as ApiSubscription
import Ellie.Api.Union.EmbedUpdate as ApiEmbedUpdate
import Ellie.Constants as Constants
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), with)
import Json.Encode as Encode exposing (Value)
import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.Revision as Revision exposing (Revision)


getRevision : Revision.Id -> Command (Result (Graphql.Http.Error ()) Revision)
getRevision revisionId =
    let
        query =
            SelectionSet.succeed identity
                |> SelectionSet.with (ApiQuery.revision arguments revisionQuery)

        arguments =
            { id = ApiScalar.PrettyId revisionId
            }

        revisionQuery =
            SelectionSet.succeed Revision
                |> SelectionSet.with ApiRevision.htmlCode
                |> SelectionSet.with ApiRevision.elmCode
                |> SelectionSet.with (ApiRevision.packages Package.selection)
                |> SelectionSet.with (ApiHelpers.defaultField "" ApiRevision.title)
                |> SelectionSet.with (ApiHelpers.versionField ApiRevision.elmVersion)
    in
    Command.GraphqlQuery
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok query
        , onError = Err
        , debounce = Nothing
        , cache = Command.Permanent
        }


runEmbed : Revision.Id -> Command (Result (Graphql.Http.Error ()) (Maybe (Maybe Error)))
runEmbed revisionId =
    let
        selection =
            SelectionSet.succeed identity
                |> SelectionSet.with (ApiMutation.runEmbed arguments embedReadySelection)

        embedReadySelection =
            SelectionSet.succeed identity
                |> SelectionSet.with (ApiEmbedReady.error Error.selection)

        arguments =
            { id = ApiScalar.PrettyId revisionId
            }
    in
    Command.GraphqlMutation
        { url = "/api"
        , token = Nothing
        , selection = SelectionSet.map Ok selection
        , onError = Err
        , debounce = Nothing
        }


embedUpdates : Revision.Id -> Subscription EmbedUpdate
embedUpdates revisionId =
    let
        selection =
            SelectionSet.succeed identity
                |> SelectionSet.with (ApiSubscription.embed arguments embedUpdateSelection)

        arguments =
            { id = ApiScalar.PrettyId revisionId
            }

        embedUpdateSelection =
            ApiEmbedUpdate.fragments
                { onEmbedReady =
                    SelectionSet.succeed EmbedUpdate.Compiled
                        |> SelectionSet.with (ApiEmbedReady.error Error.selection)
                , onEmbedFailed =
                    SelectionSet.succeed EmbedUpdate.Failed
                        |> SelectionSet.with ApiEmbedFailed.message
                }
    in
    Subscription.AbsintheSubscription
        { url = Constants.socketOrigin ++ "/api/sockets", token = Nothing }
        selection
        (\connected ->
            if connected then
                EmbedUpdate.Connected

            else
                EmbedUpdate.Disconnected
        )


goToPosition : Error.Position -> Command msg
goToPosition position =
    Command.PortSend
        { channel = "GoToPosition"
        , debounce = Nothing
        , data =
            Encode.list Encode.int
                [ position.line
                , position.column
                ]
        }
