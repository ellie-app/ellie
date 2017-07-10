module Elm.Make.ProjectGraph exposing (..)

import Data.HashDict as HashDict exposing (HashDict)
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Location as Location exposing (Location)
import Elm.Make.ProjectData as ProjectData exposing (ProjectData)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias ProjectGraph a =
    { projectData : HashDict CanonicalModule (ProjectData a)
    , projectNatives : HashDict CanonicalModule Location
    }


encoder : (a -> Value) -> ProjectGraph a -> Value
encoder inner graph =
    Encode.object
        [ ( "projectData", HashDict.encoder CanonicalModule.encoder (ProjectData.encoder inner) graph.projectData )
        , ( "projectNatives", HashDict.encoder CanonicalModule.encoder Location.encoder graph.projectNatives )
        ]


decoder : Decoder a -> Decoder (ProjectGraph a)
decoder inner =
    Decode.map2 ProjectGraph
        (Decode.field "projectData" <| HashDict.decoder CanonicalModule.decoder (ProjectData.decoder inner))
        (Decode.field "projectNatives" <| HashDict.decoder CanonicalModule.decoder Location.decoder)
