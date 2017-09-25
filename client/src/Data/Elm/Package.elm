module Data.Elm.Package exposing (..)

import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Package =
    ( Name, Version )


toString : Package -> String
toString ( name, version ) =
    Name.toString name ++ "@" ++ Version.toString version


compare : Package -> Package -> Order
compare ( ln, lv ) ( rn, rv ) =
    let
        nameCmp =
            Name.compare ln rn
    in
    if nameCmp == EQ then
        Version.compare lv rv
    else
        nameCmp


decoder : Decoder Package
decoder =
    Decode.map2 (,)
        (Decode.index 0 Name.decoder)
        (Decode.index 1 Version.decoder)


encoder : Package -> Value
encoder ( name, version ) =
    Encode.list
        [ Name.encoder name
        , Version.encoder version
        ]


docsLink : Package -> String
docsLink ( name, version ) =
    "http://package.elm-lang.org/packages/"
        ++ name.user
        ++ "/"
        ++ name.project
        ++ "/"
        ++ Version.toString version


codeLink : Package -> String
codeLink ( name, version ) =
    "http://github.com/"
        ++ name.user
        ++ "/"
        ++ name.project
        ++ "/tree/"
        ++ Version.toString version
