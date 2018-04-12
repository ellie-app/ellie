module Elm.Project
    exposing
        ( Project
        , encoder
        )

{-| Turn `elm.json` files into data that is nice to use in Elm.


# Projects

@docs Project, BrowserInfo, PackageInfo, Exposed


# JSON Conversions

@docs encode, decoder

-}

import Elm.Name as Name exposing (Name)
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Json.Encode as Encode exposing (Value)


-- PROJECT


{-| The contents of an `elm.json` with `"type": "package"`.
-}
type alias Project =
    { sourceDirs : List String
    , deps : List Package
    , elm : Version
    }



-- ENCODE


encoder : Project -> Value
encoder project =
    Encode.object
        [ ( "type", Encode.string "browser" )
        , ( "source-directories", Encode.list <| List.map Encode.string project.sourceDirs )
        , ( "elm-version", Version.encoder project.elm )
        , ( "dependencies", encodeDeps project.deps )
        , ( "test-dependencies", Encode.object [] )
        , ( "do-not-edit-this-by-hand"
          , Encode.object [ ( "transitive-dependencies", Encode.object [] ) ]
          )
        ]


encodeDeps : List Package -> Value
encodeDeps deps =
    Encode.object <| List.sortBy Tuple.first <| List.map encodeDep deps


encodeDep : Package -> ( String, Value )
encodeDep { name, version } =
    ( Name.toString name, Version.encoder version )
