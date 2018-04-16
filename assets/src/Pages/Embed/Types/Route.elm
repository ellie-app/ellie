module Pages.Embed.Types.Route exposing (Route(..), parse, toString)

import Data.Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, map, s, string)
import Data.Url.Parser.Query as QueryParser
import Data.Uuid as Uuid exposing (Uuid)
import Extra.String as String
import Navigation
import Pages.Embed.Types.Panel as Panel exposing (Panel(..))
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


type Route
    = Existing RevisionId Panel
    | NotFound


uuid : Parser (Uuid -> a) a
uuid =
    UrlParser.map Uuid.fromString string


panel : QueryParser.Parser Panel
panel =
    QueryParser.string "panel"
        |> QueryParser.map (Result.map Panel.fromString)
        |> QueryParser.map (Result.withDefault Elm)


revisionId : Parser (RevisionId -> a) a
revisionId =
    UrlParser.map RevisionId <| uuid </> int


parser : Parser (Route -> Route) Route
parser =
    UrlParser.map Existing <| s "embed" </> revisionId <?> panel


parse : Navigation.Location -> Route
parse location =
    location.href
        |> UrlParser.toUrl
        |> Maybe.andThen (UrlParser.parse parser)
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        Existing { projectId, revisionNumber } panel ->
            "/embed/" ++ Uuid.toString projectId ++ "/" ++ String.fromInt revisionNumber ++ "?panel=" ++ Panel.toString panel

        NotFound ->
            "/embed/not-found"
