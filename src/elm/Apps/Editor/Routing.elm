module Apps.Editor.Routing
    exposing
        ( Route(..)
        , parse
        , construct
        , isSpecificRevision
        )

import Navigation
import UrlParser exposing ((</>))
import Types.ProjectId as ProjectId exposing (ProjectId)


type Route
    = NewProject
    | SpecificRevision ProjectId Int
    | NotFound


parseProjectId : UrlParser.Parser (ProjectId -> b) b
parseProjectId =
    UrlParser.custom "PROJECT ID" (ProjectId.fromEncodedString >> Ok)


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map NewProject (UrlParser.s "new")
                , UrlParser.map SpecificRevision (parseProjectId </> UrlParser.int)
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
            "/" ++ (ProjectId.toEncodedString projectId) ++ "/" ++ toString revisionNumber

        NotFound ->
            "/"


isSpecificRevision : Route -> Bool
isSpecificRevision route =
    case route of
        SpecificRevision _ _ ->
            True

        _ ->
            False
