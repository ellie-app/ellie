module Pages.Embed.Types.Revision exposing (Id, Revision, editorLink, embedLink, outputLink)

import Ellie.Constants as Constants
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)


type alias Id =
    String


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    }


editorLink : Id -> String
editorLink id =
    Constants.editorBase ++ "/" ++ id


embedLink : Id -> String
embedLink id =
    Constants.embedBase ++ "/" ++ id


outputLink : Id -> String
outputLink id =
    Constants.serverOrigin ++ "/r/embed/" ++ id
