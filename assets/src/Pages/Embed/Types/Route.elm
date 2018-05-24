module Pages.Embed.Types.Route exposing (Route(..), parse, toString)

import Data.Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, map, s, string)
import Data.Url.Parser.Query as QueryParser
import Navigation
import Pages.Embed.Types.Panel as Panel exposing (Panel(..))
import Pages.Embed.Types.Revision as Revision exposing (Revision)


type Route
    = Existing Revision.Id Panel
    | NotFound


panel : QueryParser.Parser Panel
panel =
    QueryParser.string "panel"
        |> QueryParser.map (Result.map Panel.fromString)
        |> QueryParser.map (Result.withDefault Elm)


parser : Parser (Route -> Route) Route
parser =
    UrlParser.map Existing <| s "embed" </> string <?> panel


parse : Navigation.Location -> Route
parse location =
    location.href
        |> UrlParser.toUrl
        |> Maybe.andThen (UrlParser.parse parser)
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        Existing id panel ->
            "/embed/" ++ id ++ "?panel=" ++ Panel.toString panel

        NotFound ->
            "/embed/not-found"
