module Shared.Constants exposing (..)

import Types.Dependency as Dependency exposing (Dependency)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.Version as Version exposing (Version)


sidebarWidth : number
sidebarWidth =
    300


apiBase : String
apiBase =
    "%API_BASE%"


defaultDependencies : List Dependency
defaultDependencies =
    [ Dependency
        "elm-lang"
        "core"
        (VersionRange (Version 5 0 0) (Version 6 0 0))
    , Dependency
        "elm-lang"
        "html"
        (VersionRange (Version 2 0 0) (Version 3 0 0))
    ]
