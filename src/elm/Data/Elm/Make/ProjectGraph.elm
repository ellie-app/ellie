module Data.Elm.Make.ProjectGraph exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import EveryDict exposing (EveryDict)
import Data.Extra.EveryDict as EveryDict
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Elm.Make.Location as Location exposing (Location)
import Data.Elm.Make.ProjectData as ProjectData exposing (ProjectData)


type alias ProjectGraph a =
    { projectData : EveryDict CanonicalModule (ProjectData a)
    , projectNatives : EveryDict CanonicalModule Location
    }


encoder : (a -> Value) -> ProjectGraph a -> Value
encoder inner graph =
    Encode.object
        [ ( "projectData", EveryDict.encoder CanonicalModule.encoder (ProjectData.encoder inner) graph.projectData )
        , ( "projectNatives", EveryDict.encoder CanonicalModule.encoder Location.encoder graph.projectNatives )
        ]


decoder : Decoder a -> Decoder (ProjectGraph a)
decoder inner =
    Decode.map2 ProjectGraph
        (Decode.field "projectData" <| EveryDict.decoder CanonicalModule.decoder (ProjectData.decoder inner))
        (Decode.field "projectNatives" <| EveryDict.decoder CanonicalModule.decoder Location.decoder)
