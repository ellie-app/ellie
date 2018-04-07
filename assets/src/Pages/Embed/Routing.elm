module Pages.Embed.Routing
    exposing
        ( Route(..)
        , construct
        , parse
        )

import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Navigation
import UrlParser exposing ((</>))


type Route
    = SpecificRevision RevisionId
    | NotFound


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map SpecificRevision <| UrlParser.map RevisionId <| UrlParser.s "embed" </> UrlParser.string </> UrlParser.int
                ]
    in
    UrlParser.parsePath parser
        >> Maybe.withDefault NotFound


construct : Route -> String
construct route =
    case route of
        SpecificRevision { projectId, revisionNumber } ->
            "/embed/" ++ projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/embed/not-found"
