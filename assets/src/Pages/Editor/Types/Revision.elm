module Pages.Editor.Types.Revision exposing (..)

import Data.Uuid as Uuid exposing (Uuid)
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    , userId : Maybe Uuid
    }
