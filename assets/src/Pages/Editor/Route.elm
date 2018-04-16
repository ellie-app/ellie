module Pages.Editor.Route exposing (Route(..), parse, toString)

import Data.Url.Parser as UrlParser exposing ((</>), Parser, int, s, string)
import Data.Uuid as Uuid exposing (Uuid)
import Extra.String as String
import Navigation
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)


type Route
    = New
    | Existing RevisionId
    | NotFound


uuid : Parser (Uuid -> a) a
uuid =
    UrlParser.map Uuid.fromString string


parser : Parser (Route -> Route) Route
parser =
    UrlParser.oneOf
        [ UrlParser.map Existing <| UrlParser.map RevisionId <| uuid </> int
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
            "/" ++ Uuid.toString projectId ++ "/" ++ String.fromInt revisionNumber

        NotFound ->
            "/not-found"
