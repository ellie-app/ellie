module Ellie.Constants exposing (..)


packageSite : String
packageSite =
    "%PACKAGE_SITE%"


iframeId : String
iframeId =
    "results_iframe"


isProduction : Bool
isProduction =
    "%ENV%" == "production"


apiBase : String
apiBase =
    "/api"


serverOrigin : String
serverOrigin =
    "%SERVER_ORIGIN%"


editorBase : String
editorBase =
    "%SERVER_ORIGIN%"


embedBase : String
embedBase =
    "%SERVER_ORIGIN%/embed"


socketOrigin : String
socketOrigin =
    "%SOCKET_ORIGIN%"


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
