port module Pages.Embed.Effects.Outbound
    exposing
        ( Outbound(..)
        , batch
        , map
        , none
        )

import Data.Jwt as Jwt exposing (Jwt)
import Data.Uuid as Uuid exposing (Uuid)
import Elm.Docs as Docs
import Elm.Error as ElmError
import Elm.Package as Package exposing (Package)
import Elm.Project as Project exposing (Project)
import Pages.Embed.Effects.Handlers as Handlers
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


type alias ElmSource =
    String


type alias HtmlSource =
    String


type Outbound msg
    = GetRevision RevisionId (Result Handlers.GetRevisionError Revision -> msg)
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

        Batch outbounds ->
            Batch <| List.map (map f) outbounds

        None ->
            None
