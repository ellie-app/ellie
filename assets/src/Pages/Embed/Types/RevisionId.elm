module Pages.Embed.Types.RevisionId exposing (..)

import Data.Url as Url exposing (Url)
import Data.Uuid as Uuid exposing (Uuid)
import Ellie.Constants as Constants


type alias RevisionId =
    { projectId : Uuid
    , revisionNumber : Int
    }


eq : RevisionId -> RevisionId -> Bool
eq left right =
    Uuid.eq left.projectId right.projectId
        && (left.revisionNumber == right.revisionNumber)


editorLink : RevisionId -> Url
editorLink { projectId, revisionNumber } =
    Url.fromString <| Constants.editorBase ++ "/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber


embedLink : RevisionId -> Url
embedLink { projectId, revisionNumber } =
    Url.fromString <| Constants.embedBase ++ "/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber


outputLink : RevisionId -> Url
outputLink { projectId, revisionNumber } =
    Url.fromString <| Constants.serverOrigin ++ "/output/embed/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber
