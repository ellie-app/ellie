module Data.Elm.Make.ProjectData exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)


type alias ProjectData a =
    { projectLocation : a
    , projectDependencies : List CanonicalModule
    }


decoder : Decoder a -> Decoder (ProjectData a)
decoder inner =
    Decode.map2 ProjectData
        (Decode.field "projectLocation" inner)
        (Decode.field "projectDependencies" <| Decode.list CanonicalModule.decoder)


encoder : (a -> Value) -> ProjectData a -> Value
encoder inner projectData =
    Encode.object
        [ ( "projectLocation", inner projectData.projectLocation )
        , ( "projectDependencies", Encode.list <| List.map CanonicalModule.encoder projectData.projectDependencies )
        ]
