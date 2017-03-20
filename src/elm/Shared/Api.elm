module Shared.Api
    exposing
        ( send
        , toTask
        , searchPackages
        , compile
        , format
        , latestRevision
        , exactRevision
        , createProjectFromRevision
        , createRevision
        , defaultRevision
        , createGist
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



-- COMPILATION


compileExpect : Expect (List CompileError)
compileExpect =
    Decode.list CompileError.decode
        |> Http.expectJson


compile : Revision -> RequestBuilder (List CompileError)
compile revision =
    post (fullUrl "/session/compile")
        |> withApiHeaders
        |> withJsonBody (Encode.object [ ( "revision", Revision.encode revision ) ])
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


createProjectFromRevision : Revision -> RequestBuilder Revision
createProjectFromRevision revision =
    post (fullUrl "/projects")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)
        |> withJsonBody (Encode.object [ ( "revision", Revision.encode revision ) ])


createRevision : Revision -> RequestBuilder Revision
createRevision revision =
    revision.projectId
        |> Maybe.withDefault ""
        |> (\s -> put (fullUrl "/projects/" ++ s ++ "/revisions"))
        |> withApiHeaders
        |> withJsonBody (Encode.object [ ( "revision", Revision.encode revision ) ])
        |> withExpect (Http.expectJson (Decode.succeed revision))


defaultRevision : RequestBuilder Revision
defaultRevision =
    get (fullUrl "/defaults/revision")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decode)



-- EXPORTS


elmPackageJson : Revision -> String
elmPackageJson revision =
    Encode.object
        [ ( "version", Encode.string "1.0.0" )
        , ( "summary", Encode.string revision.description )
        , ( "repository", Encode.string "https://github.com/user/project.git" )
        , ( "license", Encode.string "BSD3" )
        , ( "source-directories", Encode.list [ Encode.string "." ] )
        , ( "exposed-modules", Encode.list [] )
        , ( "dependencies"
          , Encode.object <|
                List.map
                    (\p ->
                        ( p.username ++ "/" ++ p.name
                        , Encode.string <| Version.toString p.version ++ " <= v < " ++ Version.toString p.version
                        )
                    )
                    revision.packages
          )
        , ( "elm-version", Encode.string "0.18.0 <= v < 0.19.0" )
        ]
        |> Encode.encode 4


createGistPayload : Revision -> Value
createGistPayload revision =
    Encode.object
        [ ( "description", Encode.string revision.title )
        , ( "public", Encode.bool True )
        , ( "files"
          , Encode.object
                [ ( Revision.moduleName revision ++ ".elm", Encode.object [ ( "content", Encode.string revision.elmCode ) ] )
                , ( "index.html", Encode.object [ ( "content", Encode.string revision.htmlCode ) ] )
                , ( "elm-package.json", Encode.object [ ( "content", Encode.string <| elmPackageJson revision ) ] )
                ]
          )
        ]


createGistExpect : Expect String
createGistExpect =
    Decode.field "html_url" Decode.string
        |> Http.expectJson


createGist : Revision -> RequestBuilder String
createGist revision =
    post "https://api.github.com/gists"
        |> withJsonBody (createGistPayload revision)
        |> withExpect createGistExpect
