module Pages.Editor.Types.RevisionId exposing (..)

import Data.Url as Url exposing (Url)
import Ellie.Constants as Constants


type alias RevisionId =
    { projectId : String
    , revisionNumber : Int
    }


eq : RevisionId -> RevisionId -> Bool
eq left right =
    (left.projectId == right.projectId)
        && (left.revisionNumber == right.revisionNumber)


editorLink : RevisionId -> Url
editorLink { projectId, revisionNumber } =
    Url.fromString <| Constants.editorBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber


embedLink : RevisionId -> Url
embedLink { projectId, revisionNumber } =
    Url.fromString <| Constants.embedBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber
