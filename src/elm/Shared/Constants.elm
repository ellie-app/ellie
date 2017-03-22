module Shared.Constants exposing (..)

import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.Version as Version exposing (Version)


sidebarWidth : number
sidebarWidth =
    250


headerHeight : number
headerHeight =
    52


isProduction : Bool
isProduction =
    "%ENV%" == "production"


apiBase : String
apiBase =
    if isProduction then
        "%API_ORIGIN%/v%API_VERSION"
    else
        "%API_ORIGIN%"


editorBase : String
editorBase =
    "%EDITOR_BASE%"


embedBase : String
embedBase =
    "%EMBED_BASE%"


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


elmVersion : Version
elmVersion =
    Version 0 18 0


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
