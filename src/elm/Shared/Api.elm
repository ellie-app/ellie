module Shared.Api
    exposing
        ( send
        , searchPackages
        , createSession
        , removeSession
        , compile
        , format
        , addDependencies
        , latestRevision
        , exactRevision
        , createProjectFromRevision
        , createRevision
        , defaultRevision
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Http exposing (Request, Expect, Error(..))
import HttpBuilder exposing (..)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.Version as Version exposing (Version)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Uuid as Uuid exposing (Uuid)
import Types.Session as Session exposing (Session)
import Types.Dependency as Dependency exposing (Dependency)
import Types.CompileError as CompileError exposing (CompileError)
import Shared.Constants as Constants


-- TOP LEVEL


listOf : a -> List a
listOf a =
    [ a ]


send : (Result ApiError a -> msg) -> RequestBuilder a -> Cmd msg
send tagger requestBuilder =
    requestBuilder
        |> toRequest
        |> Http.send (Result.mapError upgradeError >> tagger)


fullUrl : String -> String
fullUrl path =
    Constants.apiBase ++ path


withApiHeaders : RequestBuilder a -> RequestBuilder a
withApiHeaders builder =
    builder
        |> withCredentials
        |> withHeader "Accept" "application/json"



-- ERRORS


upgradeError : Error -> ApiError
upgradeError error =
    case error of
        BadUrl url ->
            { statusCode = 0
            , message = "Bad Url" ++ url
            , explanation = Just <| "Url " ++ url
            }

        NetworkError ->
            { statusCode = 0
            , message = "A Network Error Occurred"
            , explanation = Nothing
            }

        Timeout ->
            { statusCode = 0
            , message = "A Network Timeout Occurred"
            , explanation = Nothing
            }

        BadStatus response ->
            { statusCode = response.status.code
            , message = response.status.message
            , explanation = Just response.body
            }

        BadPayload error response ->
            { statusCode = response.status.code
            , message = "Unexpected Response from Server"
            , explanation = Just response.body
            }



-- SEARCH


searchPackages : Version -> String -> RequestBuilder (List PackageSearchResult)
searchPackages elmVersion searchTerm =
    get (fullUrl "/packages/search")
        |> withQueryParams
            [ ( "query", searchTerm )
            , ( "elmVersion", Version.toString elmVersion )
            ]
        |> withApiHeaders
        |> withExpect (Http.expectJson (Decode.list PackageSearchResult.decode))



-- SESSIONS


createSession : RequestBuilder Session
createSession =
    post (fullUrl "/sessions")
        |> withApiHeaders
        |> withExpect (Http.expectJson Session.decode)


removeSession : Session -> RequestBuilder ()
removeSession session =
    delete ("http://localhost:1337/sessions/" ++ session.id)
        |> withApiHeaders



-- COMPILATION


compilePayload : String -> Value
compilePayload source =
    Encode.object
        [ ( "source", Encode.string source )
        ]


compileExpect : Expect (List CompileError)
compileExpect =
    Decode.list CompileError.decode
        |> Http.expectJson


compile : String -> Session -> RequestBuilder (List CompileError)
compile source session =
    post (fullUrl ("/sessions/" ++ session.id ++ "/compile"))
        |> withApiHeaders
        |> withJsonBody (compilePayload source)
        |> withExpect compileExpect


formatPayload : String -> Value
formatPayload source =
    Encode.object
        [ ( "source", Encode.string source ) ]


formatExpect : Expect String
formatExpect =
    Decode.field "source" Decode.string
        |> Http.expectJson


format : String -> RequestBuilder String
format source =
    post (fullUrl "/format")
        |> withApiHeaders
        |> withJsonBody (formatPayload source)
        |> withExpect formatExpect



-- DEPENDENCIES


addDependenciesPayload : List Dependency -> Value
addDependenciesPayload dependencies =
    dependencies
        |> List.map Dependency.encode
        |> Encode.list
        |> (,) "dependencies"
        |> listOf
        |> Encode.object


addDependencies : Session -> List Dependency -> RequestBuilder ()
addDependencies session dependencies =
    put (fullUrl ("/sessions/" ++ session.id ++ "/dependencies"))
        |> withApiHeaders
        |> withJsonBody (addDependenciesPayload dependencies)



-- REVISIONS AND PROJECTS


latestRevision : Uuid -> RequestBuilder Revision
latestRevision projectId =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/latest"))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)


exactRevision : Uuid -> Int -> RequestBuilder Revision
exactRevision projectId revisionNumber =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/" ++ toString revisionNumber))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)


createProjectFromRevision : Revision -> RequestBuilder Revision
createProjectFromRevision revision =
    post (fullUrl "/projects")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)
        |> withJsonBody (Revision.encode revision)


createRevision : Revision -> RequestBuilder Revision
createRevision revision =
    revision.projectId
        |> Maybe.map Uuid.toString
        |> Maybe.withDefault ""
        |> (\s -> put (fullUrl "/projects/" ++ s ++ "/revisions"))
        |> withApiHeaders
        |> withJsonBody (Revision.encode revision)
        |> withExpect (Http.expectJson (Decode.succeed revision))


defaultRevision : RequestBuilder Revision
defaultRevision =
    get (fullUrl "/defaults/revision")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)
