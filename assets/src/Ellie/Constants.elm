module Ellie.Constants exposing (apiBase, editorBase, embedBase, headerHeight, hostname, iframeId, isProduction, packageSite, sansFont, scriptFont, secure, serverOrigin, sidebarWidth, socketOrigin)


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


hostname : String
hostname =
    "%HOSTNAME%"


secure : Bool
secure =
    "%PROTOCOL%" == "https:"


serverOrigin : String
serverOrigin =
    if secure then
        "https://" ++ hostname

    else
        "http://" ++ hostname


editorBase : String
editorBase =
    serverOrigin


embedBase : String
embedBase =
    serverOrigin ++ "/embed"


socketOrigin : String
socketOrigin =
    if secure then
        "wss://" ++ hostname

    else
        "ws://" ++ hostname


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
