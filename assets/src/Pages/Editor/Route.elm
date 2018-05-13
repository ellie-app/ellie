module Pages.Editor.Route exposing (Route(..), parse, toString)

import Data.Url.Parser as UrlParser exposing ((</>), Parser, int, s, string)
import Extra.String as String
import Navigation
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)


type Route
    = New
    | Existing RevisionId
    | NotFound


parser : Parser (Route -> Route) Route
parser =
    UrlParser.oneOf
        [ UrlParser.map Existing <| UrlParser.map RevisionId <| string </> int
        , UrlParser.map New <| s "new"
        ]


parse : Navigation.Location -> Route
parse location =
    location.href
        |> UrlParser.toUrl
        |> Maybe.andThen (UrlParser.parse parser)
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
