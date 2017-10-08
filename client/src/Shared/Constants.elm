module Shared.Constants exposing (..)


sidebarWidth : number
sidebarWidth =
    240


headerHeight : number
headerHeight =
    40


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


assetBase : String
assetBase =
    if isProduction then
        cdnBase ++ "/assets/"
    else
        "/"


asset : String -> String
asset relative =
    assetBase ++ relative


scriptFont : String
scriptFont =
    "Leckerli One"


sansFont : String
sansFont =
    "Quicksand"


carbonZoneId : String
carbonZoneId =
    "%CARBON_ZONE_ID%"


carbonServe : String
carbonServe =
    "%CARBON_SERVE%"


carbonPlacement : String
carbonPlacement =
    "%CARBON_PLACEMENT%"
