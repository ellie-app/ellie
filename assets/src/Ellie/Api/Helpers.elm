module Ellie.Api.Helpers exposing (..)

import Data.Uuid as Uuid exposing (Uuid)
import Ellie.Api.Scalar as ApiScalar
import Elm.Name as Name exposing (Name)
import Elm.Version as Version exposing (Version)
import Graphqelm.Field as Field exposing (Field)
import Graphqelm.Http exposing (Request)


versionField : Field ApiScalar.Version a -> Field Version a
versionField =
    Field.mapOrFail <|
        \(ApiScalar.Version string) -> Version.fromString string


uuidField : Field ApiScalar.Uuid a -> Field Uuid a
uuidField =
    Field.map <|
        \(ApiScalar.Uuid string) -> Uuid.fromString string


projectIdField : Field ApiScalar.ProjectId a -> Field String a
projectIdField =
    Field.map <|
        \(ApiScalar.ProjectId string) -> string


nameField : Field ApiScalar.Name a -> Field Name a
nameField =
    Field.mapOrFail <|
        \(ApiScalar.Name string) -> Name.fromString string


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
