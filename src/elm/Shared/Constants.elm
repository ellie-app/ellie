module Shared.Constants exposing (..)

import Types.Dependency as Dependency exposing (Dependency)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.Version as Version exposing (Version)


sidebarWidth : number
sidebarWidth =
    300


headerHeight : number
headerHeight =
    50


apiBase : String
apiBase =
    "%API_BASE%"


editorBase : String
editorBase =
    "%EDITOR_BASE%"


embedBase : String
embedBase =
    "%EMBED_BASE%"


cdnBase : String
cdnBase =
    "%CDN_BASE%"


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


elmVersion : Version
elmVersion =
    Version 0 18 0


scriptFont : String
scriptFont =
    "Leckerli One"


sansFont : String
sansFont =
    "Quicksand"
