module Shared.Api
    exposing
        ( send
        , searchPackages
        , createNewSession
        , createSessionForRevision
        , removeSession
        , compile
        , format
        , addDependencies
        , removeDependency
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


expectValue : a -> Expect a
expectValue value =
    Http.expectStringResponse (\_ -> Ok value)



-- ERRORS


upgradeError : Error -> ApiError
upgradeError error =
    case error of
        BadUrl url ->
            { statusCode = 0
            , message = "Bad Url" ++ url
            , explanation = "Url " ++ url
            }

        NetworkError ->
            { statusCode = 0
            , message = "A Network Error Occurred"
            , explanation = "The network may be offline or your signal may not be strong enough."
            }

        Timeout ->
            { statusCode = 0
            , message = "A Network Timeout Occurred"
            , explanation = "The request timed out."
            }

        BadStatus response ->
            { statusCode = response.status.code
            , message = response.status.message
            , explanation = response.body
            }

        BadPayload error response ->
            { statusCode = response.status.code
            , message = "Unexpected Response from Server"
            , explanation = response.body
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


createNewSession : RequestBuilder Session
createNewSession =
    post (fullUrl "/sessions")
        |> withApiHeaders
        |> withExpect (Http.expectJson Session.decode)


sessionForRevisionPayload : Uuid -> Int -> Value
sessionForRevisionPayload projectId revisionNumber =
    Encode.object
        [ ( "projectId", Uuid.encode projectId )
        , ( "revisionNumber", Encode.int revisionNumber )
        ]


createSessionForRevision : Uuid -> Int -> RequestBuilder Session
createSessionForRevision projectId revisionNumber =
    post (fullUrl "/sessions")
        |> withApiHeaders
        |> withExpect (Http.expectJson Session.decode)
        |> withJsonBody (sessionForRevisionPayload projectId revisionNumber)


removeSession : Session -> RequestBuilder ()
removeSession session =
    delete (fullUrl ("/sessions/" ++ session.id))
        |> withApiHeaders



-- COMPILATION


compilePayload : String -> String -> Value
compilePayload elmCode htmlCode =
    Encode.object
        [ ( "elmCode", Encode.string elmCode )
        , ( "htmlCode", Encode.string htmlCode )
        ]


compileExpect : Expect (List CompileError)
compileExpect =
    Decode.list CompileError.decode
        |> Http.expectJson


compile : String -> String -> Session -> RequestBuilder (List CompileError)
compile elmCode htmlCode session =
    post (fullUrl ("/sessions/" ++ session.id ++ "/compile"))
        |> withApiHeaders
        |> withJsonBody (compilePayload elmCode htmlCode)
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


removeDependency : Session -> Dependency -> RequestBuilder Dependency
removeDependency session dependency =
    delete (fullUrl ("/sessions/" ++ session.id ++ "/dependencies"))
        |> withApiHeaders
        |> withJsonBody (Dependency.encode dependency)
        |> withExpect (expectValue dependency)



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
