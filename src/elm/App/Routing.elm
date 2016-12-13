module App.Routing exposing (..)

import Navigation
import UrlParser exposing ((</>))
import Shared.Uuid as Uuid exposing (Uuid)


type Route
    = NewProject
    | LatestRevision Uuid
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
                , UrlParser.map LatestRevision uuid
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

        LatestRevision projectId ->
            "/" ++ Uuid.toString projectId

        SpecificRevision projectId revisionNumber ->
            "/" ++ Uuid.toString projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/"
