module App.Routing
    exposing
        ( Route(..)
        , parse
        , construct
        )

import Navigation
import UrlParser exposing ((</>))
import Types.Uuid as Uuid exposing (Uuid)


type Route
    = NewProject
    | SpecificRevision Uuid Int
    | NotFound


uuid : UrlParser.Parser (Uuid -> a) a
uuid =
    UrlParser.custom "UUID" Uuid.uuid


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map NewProject (UrlParser.s "new")
                , UrlParser.map SpecificRevision (uuid </> UrlParser.int)
                ]
    in
        UrlParser.parsePath parser
            >> Maybe.withDefault NotFound


construct : Route -> String
construct route =
    case route of
        NewProject ->
            "/new"

        SpecificRevision projectId revisionNumber ->
            "/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/"
