module Apps.Embed.Routing
    exposing
        ( Route(..)
        , parse
        , construct
        )

import Navigation
import UrlParser exposing ((</>))


type Route
    = SpecificRevision String Int
    | NotFound


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map SpecificRevision <| UrlParser.string </> UrlParser.int
                ]
    in
        UrlParser.parsePath parser
            >> Maybe.withDefault NotFound


construct : Route -> String
construct route =
    case route of
        SpecificRevision projectId revisionNumber ->
            "/" ++ projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/"
