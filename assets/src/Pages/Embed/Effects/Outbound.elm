port module Pages.Embed.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        )

import Elm.Error as Error exposing (Error)
import Pages.Embed.Effects.Handlers as Handlers
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


type alias ElmSource =
    String


type alias HtmlSource =
    String


type Outbound msg
    = GetRevision RevisionId (Result Handlers.GetRevisionError Revision -> msg)
    | RunEmbed RevisionId (Result Handlers.RunEmbedError (Maybe (Maybe Error)) -> msg)
    | Batch (List (Outbound msg))
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
        GetRevision id callback ->
            GetRevision id (callback >> f)

        RunEmbed revisionId callback ->
            RunEmbed revisionId (callback >> f)

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
