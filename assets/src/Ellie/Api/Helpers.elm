module Ellie.Api.Helpers exposing (defaultField, nameField, projectIdField, unitField, versionField, withMaybe)

import Ellie.Api.Scalar as ApiScalar
import Elm.Name as Name exposing (Name)
import Elm.Version as Version exposing (Version)
import Graphql.Field as Field exposing (Field)
import Graphql.Http exposing (Request)


versionField : Field ApiScalar.ElmVersion a -> Field Version a
versionField =
    Field.mapOrFail <|
        \(ApiScalar.ElmVersion string) -> Version.fromString string


unitField : Field ApiScalar.Unit a -> Field () a
unitField =
    Field.map <| \_ -> ()


projectIdField : Field ApiScalar.PrettyId a -> Field String a
projectIdField =
    Field.map <|
        \(ApiScalar.PrettyId string) -> string


nameField : Field ApiScalar.ElmName a -> Field Name a
nameField =
    Field.mapOrFail <|
        \(ApiScalar.ElmName string) -> Name.fromString string


defaultField : a -> Field (Maybe a) b -> Field a b
defaultField default =
    Field.map (Maybe.withDefault default)


withMaybe : (a -> Request b -> Request b) -> Maybe a -> Request b -> Request b
withMaybe f maybe request =
    case maybe of
        Just a ->
            f a request

        Nothing ->
            request
