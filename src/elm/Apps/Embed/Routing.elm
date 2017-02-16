module Apps.Embed.Routing
    exposing
        ( Route(..)
        , parse
        , construct
        )

import Navigation
import UrlParser exposing ((</>))
import Types.ProjectId as ProjectId exposing (ProjectId)


type Route
    = SpecificRevision ProjectId Int
    | NotFound


parseProjectId : UrlParser.Parser (ProjectId -> b) b
parseProjectId =
    UrlParser.custom "PROJECT ID" (ProjectId.fromEncodedString >> Ok)


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map SpecificRevision <| parseProjectId </> UrlParser.int
                ]
    in
        UrlParser.parsePath parser
            >> Maybe.withDefault NotFound


construct : Route -> String
construct route =
    case route of
        SpecificRevision projectId revisionNumber ->
            "/" ++ ProjectId.toEncodedString projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/not-found"
