module Pages.Embed.Types.Route exposing (Route(..), parse, toString)

import Pages.Embed.Types.Panel as Panel exposing (Panel(..))
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, map, s, string)
import Url.Parser.Query as QueryParser


type Route
    = Existing Revision.Id Panel
    | NotFound


panel : QueryParser.Parser Panel
panel =
    QueryParser.string "panel"
        |> QueryParser.map (Maybe.map Panel.fromString)
        |> QueryParser.map (Maybe.withDefault Elm)


parser : Parser (Route -> Route) Route
parser =
    UrlParser.map Existing <| s "embed" </> string <?> panel


parse : Url -> Route
parse url =
    UrlParser.parse parser url
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        Existing id existingPanel ->
            "/embed/" ++ id ++ "?panel=" ++ Panel.toString existingPanel

        NotFound ->
            "/embed/not-found"
