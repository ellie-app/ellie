module Pages.Embed.Types.Revision exposing (..)

import Data.Url as Url exposing (Url)
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


editorLink : Id -> Url
editorLink id =
    Url.fromString <| Constants.editorBase ++ "/" ++ id


embedLink : Id -> Url
embedLink id =
    Url.fromString <| Constants.embedBase ++ "/" ++ id


outputLink : Id -> Url
outputLink id =
    Url.fromString <| Constants.serverOrigin ++ "/r/embed/" ++ id
