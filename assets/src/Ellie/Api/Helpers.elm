module Ellie.Api.Helpers exposing (defaultField, nameField, projectIdField, unitField, versionField, withMaybe)

import Ellie.Api.Scalar as ApiScalar
import Elm.Name as Name exposing (Name)
import Elm.Version as Version exposing (Version)
import Graphql.Http exposing (Request)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


versionField : SelectionSet ApiScalar.ElmVersion a -> SelectionSet Version a
versionField =
    SelectionSet.mapOrFail <|
        \(ApiScalar.ElmVersion string) -> Version.fromString string


unitField : SelectionSet ApiScalar.Unit a -> SelectionSet () a
unitField =
    SelectionSet.map <| \_ -> ()


projectIdField : SelectionSet ApiScalar.PrettyId a -> SelectionSet String a
projectIdField =
    SelectionSet.map <|
        \(ApiScalar.PrettyId string) -> string


nameField : SelectionSet ApiScalar.ElmName a -> SelectionSet Name a
nameField =
    SelectionSet.mapOrFail <|
        \(ApiScalar.ElmName string) -> Name.fromString string


defaultField : a -> SelectionSet (Maybe a) b -> SelectionSet a b
defaultField default =
    SelectionSet.map (Maybe.withDefault default)


withMaybe : (a -> Request b -> Request b) -> Maybe a -> Request b -> Request b
withMaybe f maybe request =
    case maybe of
        Just a ->
            f a request

        Nothing ->
            request
