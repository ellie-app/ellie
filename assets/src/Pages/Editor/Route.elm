module Pages.Editor.Route exposing (Route(..), parse, toString)

import Data.Url.Parser as UrlParser exposing ((</>), Parser, int, s, string)
import Navigation
import Pages.Editor.Types.Revision as Revision exposing (Revision)


type Route
    = New
    | Existing Revision.Id
    | NotFound


revisionId : Parser (Revision.Id -> a) a
revisionId =
    UrlParser.custom "REVISION_ID" <|
        \string ->
            if String.endsWith "a1" string then
                Just string
            else
                Nothing


parser : Parser (Route -> Route) Route
parser =
    UrlParser.oneOf
        [ UrlParser.map New <| s "new"
        , UrlParser.map Existing revisionId
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

        Existing revisionId ->
            "/" ++ revisionId

        NotFound ->
            "/not-found"
