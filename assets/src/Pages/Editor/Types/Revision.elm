module Pages.Editor.Types.Revision exposing (..)

import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    }
