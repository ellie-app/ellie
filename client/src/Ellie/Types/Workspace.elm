module Ellie.Types.Workspace exposing (..)

import Elm.Package as Package exposing (Package)


type alias Workspace =
    { elmCode : String
    , htmlCode : String
    , packages : List Package
    }
