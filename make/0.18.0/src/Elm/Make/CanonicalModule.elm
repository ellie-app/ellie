module Elm.Make.CanonicalModule exposing (..)

import Elm.Compiler.Module as Module
import Elm.Package as Package exposing (Package)
import Extra.String as String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias CanonicalModule =
    { package : Package
    , name : Module.Raw
    }


hash : CanonicalModule -> Int
hash { package, name } =
    String.hash <| Package.toString package ++ "_" ++ Module.nameToString name


compare : CanonicalModule -> CanonicalModule -> Order
compare left right =
    let
        packageCmp =
            Package.compare left.package right.package
    in
    if packageCmp == EQ then
        Basics.compare left.name right.name
    else
        packageCmp


decoder : Decoder CanonicalModule
decoder =
    Decode.map2 CanonicalModule
        (Decode.field "package" Package.decoder)
        (Decode.field "name" Module.nameDecoder)


encoder : CanonicalModule -> Value
encoder modul =
    Encode.object
        [ ( "package", Package.encoder modul.package )
        , ( "name", Module.nameEncoder modul.name )
        ]


toVarString : CanonicalModule -> String
toVarString modul =
    let
        ( packageName, version ) =
            modul.package

        safeUser =
            String.replace "-" "_" packageName.user

        safeProject =
            String.replace "-" "_" packageName.project

        safeModuleName =
            String.join "_" modul.name
    in
    "_" ++ safeUser ++ "$" ++ safeProject ++ "$" ++ safeModuleName


qualifiedVar : CanonicalModule -> String -> String
qualifiedVar modul var =
    toVarString modul ++ "$" ++ String.replace "'" "$" var
