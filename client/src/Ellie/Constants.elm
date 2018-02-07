module Ellie.Constants exposing (..)


iframeId : String
iframeId =
    "results_iframe"


isProduction : Bool
isProduction =
    "%ENV%" == "production"


apiBase : String
apiBase =
    "/api"


editorBase : String
editorBase =
    "%SERVER_ORIGIN%"


embedBase : String
embedBase =
    "%SERVER_ORIGIN%/embed"


cdnBase : String
cdnBase =
    "%CDN_BASE%"


workspaceUrl : String
workspaceUrl =
    "ws://localhost:1337/workspace"


assetBase : String
assetBase =
    if isProduction then
        cdnBase ++ "/assets/"
    else
        "/"


asset : String -> String
asset relative =
    assetBase ++ relative


sidebarWidth : number
sidebarWidth =
    250


headerHeight : number
headerHeight =
    52


scriptFont : String
scriptFont =
    "Leckerli One"


sansFont : String
sansFont =
    "Quicksand"
