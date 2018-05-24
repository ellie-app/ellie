module Elm.Package
    exposing
        ( Package
        , codeLink
        , compare
        , decoder
        , docsLink
        , encoder
        , selection
        , toInputObject
        , toString
        )

import Ellie.Api.Helpers as ApiHelpers
import Ellie.Api.InputObject as ApiInputObject
import Ellie.Api.Object as ApiObject
import Ellie.Api.Object.ElmPackage as ApiPackage
import Ellie.Api.Scalar as ApiScalar
import Elm.Name as Name exposing (Name)
import Elm.Version as Version exposing (Version)
import Graphqelm.SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Package =
    { name : Name
    , version : Version
    }


decoder : Decoder Package
decoder =
    Decode.map2 Package
        (Decode.field "name" Name.decoder)
        (Decode.field "version" Version.decoder)


encoder : Package -> Value
encoder package =
    Encode.object
        [ ( "name", Name.encoder package.name )
        , ( "version", Version.encoder package.version )
        ]


toString : Package -> String
toString { name, version } =
    Name.toString name ++ "@" ++ Version.toString version


compare : Package -> Package -> Order
compare l r =
    case Name.compare l.name r.name of
        EQ ->
            Version.compare l.version r.version

        nameCmp ->
            nameCmp


docsLink : Package -> String
docsLink { name, version } =
    "http://package.elm-lang.org/packages/"
        ++ name.user
        ++ "/"
        ++ name.project
        ++ "/"
        ++ Version.toString version


codeLink : Package -> String
codeLink { name, version } =
    "https://github.com/"
        ++ name.user
        ++ "/"
        ++ name.project
        ++ "/tree/"
        ++ Version.toString version


selection : SelectionSet Package ApiObject.ElmPackage
selection =
    ApiPackage.selection Package
        |> with (ApiHelpers.nameField ApiPackage.name)
        |> with (ApiHelpers.versionField ApiPackage.version)


toInputObject : Package -> ApiInputObject.ElmPackageInput
toInputObject package =
    { name = ApiScalar.ElmName <| Name.toString package.name
    , version = ApiScalar.ElmVersion <| Version.toString package.version
    }
