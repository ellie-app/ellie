module Pages.Editor.Route exposing (Route(..), parse, toString)

import Elm.Compiler as Compiler
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Extra.Maybe as Maybe
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, s, string)
import Url.Parser.Query as QueryParser


type Route
    = New
    | Existing Revision.Id
    | Example Revision
    | NotFound


revisionId : Parser (Revision.Id -> a) a
revisionId =
    UrlParser.custom "REVISION_ID" <|
        \string ->
            if String.endsWith "a1" string then
                Just string

            else
                Nothing


exampleRevision : Revision
exampleRevision =
    { htmlCode = "hi"
    , elmCode = "hello"
    , packages = []
    , title = "hi"
    , elmVersion = Version 0 19 0
    }


elmVersion : QueryParser.Parser Version
elmVersion =
    QueryParser.custom "elmversion" <|
        \strings ->
            case strings of
                [ string ] ->
                    string
                        |> Version.fromString
                        |> Result.toMaybe
                        |> Maybe.withDefault Compiler.version

                _ ->
                    Compiler.version


elmCode : QueryParser.Parser String
elmCode =
    QueryParser.string "elmcode"
        |> QueryParser.map (Maybe.withDefault "")


htmlCode : QueryParser.Parser String
htmlCode =
    QueryParser.string "htmlcode"
        |> QueryParser.map (Maybe.withDefault "")


packages : QueryParser.Parser (List Package)
packages =
    QueryParser.custom "packages" <|
        \strings ->
            strings
                |> List.map Package.fromString
                |> Maybe.combine
                |> Maybe.withDefault []


title : QueryParser.Parser String
title =
    QueryParser.string "title"
        |> QueryParser.map (Maybe.withDefault "")


revision : QueryParser.Parser Revision
revision =
    QueryParser.map5 Revision
        htmlCode
        elmCode
        packages
        title
        elmVersion


example : Parser (Route -> a) a
example =
    UrlParser.map Example (s "a" </> s "example" </> s "v1" <?> revision)


parser : Parser (Route -> Route) Route
parser =
    UrlParser.oneOf
        [ UrlParser.map New <| s "new"
        , UrlParser.map Existing revisionId
        , example
        ]


parse : Url -> Route
parse url =
    UrlParser.parse parser url
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        New ->
            "/new"

        Existing revisionId ->
            "/" ++ revisionId

        Example _ ->
            "/a/example/v1"

        NotFound ->
            "/not-found"
