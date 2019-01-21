module Pages.Embed.Types.Revision exposing (Id, Revision, editorLink, embedLink, outputLink)

import Ellie.Constants as Constants
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Url as Url exposing (Url)


type alias Id =
    String


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    }


editorLink : Id -> Maybe Url
editorLink id =
    Url.fromString <| Constants.editorBase ++ "/" ++ id


embedLink : Id -> Maybe Url
embedLink id =
    Url.fromString <| Constants.embedBase ++ "/" ++ id


outputLink : Id -> Maybe Url
outputLink id =
    Url.fromString <| Constants.serverOrigin ++ "/r/embed/" ++ id
