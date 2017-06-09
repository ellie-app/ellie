module Apps.Editor.Routing
    exposing
        ( Route(..)
        , parse
        , construct
        , isSpecificRevision
        )

import Navigation
import UrlParser exposing ((</>))
import Data.Ellie.RevisionId exposing (RevisionId)


type Route
    = NewProject
    | SpecificRevision RevisionId
    | NotFound


parse : Navigation.Location -> Route
parse =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map NewProject (UrlParser.s "new")
                , UrlParser.map SpecificRevision <| UrlParser.map RevisionId (UrlParser.string </> UrlParser.int)
                ]
    in
        UrlParser.parsePath parser
            >> Maybe.withDefault NotFound


construct : Route -> String
construct route =
    case route of
        NewProject ->
            "/new"

        SpecificRevision { projectId, revisionNumber } ->
            "/" ++ projectId ++ "/" ++ toString revisionNumber

        NotFound ->
            "/"


isSpecificRevision : Route -> Bool
isSpecificRevision route =
    case route of
        SpecificRevision _ ->
            True

        _ ->
            False
