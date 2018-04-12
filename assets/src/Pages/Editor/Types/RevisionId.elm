module Pages.Editor.Types.RevisionId exposing (..)

import Data.Url as Url exposing (Url)
import Data.Uuid as Uuid exposing (Uuid)


type alias RevisionId =
    { projectId : Uuid
    , revisionNumber : Int
    }


eq : RevisionId -> RevisionId -> Bool
eq left right =
    Uuid.eq left.projectId right.projectId
        && (left.revisionNumber == right.revisionNumber)


link : RevisionId -> Url
link { projectId, revisionNumber } =
    Url.fromString <| "http://localhost:4000/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber
