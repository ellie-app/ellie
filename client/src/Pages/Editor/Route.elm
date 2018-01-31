module Pages.Editor.Route exposing (Route(..), parse, toString)

import Ellie.Types.Revision as Revision
import Extra.String as String
import Navigation
import UrlParser exposing ((</>), Parser, int, map, s, string)


type Route
    = New
    | Existing Revision.Id
    | NotFound


parser : Parser (Route -> Route) Route
parser =
    UrlParser.oneOf
        [ UrlParser.map Existing <| UrlParser.map Revision.Id <| string </> int
        , UrlParser.map New <| s "new"
        ]


parse : Navigation.Location -> Route
parse location =
    UrlParser.parsePath parser location
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        New ->
            "/new"

        Existing { projectId, revisionNumber } ->
            "/" ++ projectId ++ "/" ++ String.fromInt revisionNumber

        NotFound ->
            "/not-found"
