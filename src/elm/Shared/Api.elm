module Shared.Api
    exposing
        ( send
        , toTask
        , searchPackages
        , createNewSession
        , createSessionForRevision
        , removeSession
        , refreshSession
        , compile
        , writeIframe
        , format
        , addDependencies
        , removeDependency
        , latestRevision
        , exactRevision
        , createProjectFromRevision
        , createRevision
        , defaultRevision
        )

import Task exposing (Task)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Http exposing (Request, Expect, Error(..))
import HttpBuilder exposing (..)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.Version as Version exposing (Version)
import Types.Package as Package exposing (Package)
import Types.Session as Session exposing (Session)
import Types.Dependency as Dependency exposing (Dependency)
import Types.CompileError as CompileError exposing (CompileError)
import Shared.Constants as Constants


-- TOP LEVEL


listOf : a -> List a
listOf a =
    [ a ]


send : (Result ApiError a -> msg) -> RequestBuilder a -> Cmd msg
send tagger builder =
    builder
        |> toRequest
        |> Http.send (Result.mapError upgradeError >> tagger)


toTask : RequestBuilder a -> Task ApiError a
toTask builder =
    builder
        |> HttpBuilder.toTask
        |> Task.mapError upgradeError


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


decodeServerError : Decoder String
decodeServerError =
    Decode.oneOf
        [ Decode.field "message" Decode.string
        , Decode.field "error" Decode.string
        ]


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
            case Decode.decodeString decodeServerError response.body of
                Ok explanation ->
                    { statusCode = response.status.code
                    , message = response.status.message
                    , explanation = explanation
                    }

                Err failure ->
                    upgradeError <| BadPayload failure response

        BadPayload innerError response ->
            { statusCode = response.status.code
            , message = "Unexpected Response from Server"
            , explanation = innerError
            }



-- SEARCH


searchPackages : Version -> String -> RequestBuilder (List Package)
searchPackages elmVersion searchTerm =
    get (fullUrl "/packages/search")
        |> withQueryParams
            [ ( "query", searchTerm )
            , ( "elmVersion", Version.toString elmVersion )
            ]
        |> withApiHeaders
        |> withExpect (Http.expectJson (Decode.list Package.decode))



-- SESSIONS


createNewSession : RequestBuilder Session
createNewSession =
    post (fullUrl "/sessions")
        |> withApiHeaders
        |> withExpect (Http.expectJson Session.decode)


sessionForRevisionPayload : String -> Int -> Value
sessionForRevisionPayload projectId revisionNumber =
    Encode.object
        [ ( "projectId", Encode.string projectId )
        , ( "revisionNumber", Encode.int revisionNumber )
        ]


createSessionForRevision : String -> Int -> RequestBuilder Session
createSessionForRevision projectId revisionNumber =
    post (fullUrl "/sessions")
        |> withApiHeaders
        |> withExpect (Http.expectJson Session.decode)
        |> withJsonBody (sessionForRevisionPayload projectId revisionNumber)


removeSession : Session -> RequestBuilder ()
removeSession session =
    delete (fullUrl ("/sessions/" ++ session.id))
        |> withApiHeaders


refreshSession : Session -> RequestBuilder ()
refreshSession session =
    post (fullUrl ("/sessions/" ++ session.id ++ "/refresh"))
        |> withApiHeaders



-- COMPILATION


compilePayload : String -> Value
compilePayload elmCode =
    Encode.object
        [ ( "elmCode", Encode.string elmCode ) ]


compileExpect : Expect (List CompileError)
compileExpect =
    Decode.list CompileError.decode
        |> Http.expectJson


compile : String -> Session -> RequestBuilder (List CompileError)
compile elmCode session =
    post (fullUrl ("/sessions/" ++ session.id ++ "/compile"))
        |> withApiHeaders
        |> withJsonBody (compilePayload elmCode)
        |> withExpect compileExpect


writeIframePayload : String -> Value
writeIframePayload htmlCode =
    Encode.object
        [ ( "htmlCode", Encode.string htmlCode ) ]


writeIframe : String -> Session -> RequestBuilder ()
writeIframe htmlCode session =
    post (fullUrl ("/sessions/" ++ session.id ++ "/iframe"))
        |> withApiHeaders
        |> withJsonBody (writeIframePayload htmlCode)
        |> withExpect (expectValue ())


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


latestRevision : String -> RequestBuilder Revision
latestRevision projectId =
    get (fullUrl ("/projects/" ++ projectId ++ "/revisions/latest"))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)


exactRevision : String -> Int -> RequestBuilder Revision
exactRevision projectId revisionNumber =
    get (fullUrl ("/projects/" ++ projectId ++ "/revisions/" ++ toString revisionNumber))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)


createProjectFromRevision : Session -> Revision -> RequestBuilder Revision
createProjectFromRevision session revision =
    post (fullUrl ("/projects?sessionId=" ++ session.id))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)
        |> withJsonBody (Revision.encode revision)


createRevision : Session -> Revision -> RequestBuilder Revision
createRevision session revision =
    revision.projectId
        |> Maybe.withDefault ""
        |> (\s -> put (fullUrl "/projects/" ++ s ++ "/revisions?sessionId=" ++ session.id))
        |> withApiHeaders
        |> withJsonBody (Revision.encode revision)
        |> withExpect (Http.expectJson (Decode.succeed revision))


defaultRevision : RequestBuilder Revision
defaultRevision =
    get (fullUrl "/defaults/revision")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)
