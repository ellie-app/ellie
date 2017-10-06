module Shared.Api
    exposing
        ( acceptTerms
        , createGist
        , defaultRevision
        , exactRevision
        , format
        , searchPackages
        , send
        , termsContent
        , toTask
        , uploadSignatures
        )

import Data.Aws.UploadSignature as UploadSignature exposing (UploadSignature)
import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision, Snapshot(..))
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Description as Description exposing (Description)
import Data.Elm.Package.Version as Version exposing (Version)
import Http exposing (Error(..), Expect, Request)
import Http.Extra as Http
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Shared.Constants as Constants
import Task exposing (Task)


-- TOP LEVEL


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
    get (fullUrl "/search")
        |> withQueryParams
            [ ( "query", searchTerm )
            , ( "elmVersion", Version.toString elmVersion )
            ]
        |> withApiHeaders
        |> withExpect (Http.expectJson (Decode.list Package.decoder))



-- FORMAT


formatPayload : String -> Value
formatPayload source =
    Encode.object
        [ ( "source", Encode.string source ) ]


formatExpect : Expect String
formatExpect =
    Decode.field "result" Decode.string
        |> Http.expectJson


format : String -> RequestBuilder String
format source =
    post (fullUrl "/format")
        |> withApiHeaders
        |> withJsonBody (formatPayload source)
        |> withExpect formatExpect



-- REVISIONS AND PROJECTS


exactRevision : RevisionId -> RequestBuilder Revision
exactRevision { projectId, revisionNumber } =
    get (fullUrl ("/revisions/" ++ projectId ++ "/" ++ toString revisionNumber))
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decoder)


defaultRevision : RequestBuilder Revision
defaultRevision =
    get (fullUrl "/revisions/default")
        |> withApiHeaders
        |> withExpect (Http.expectJson Revision.decoder)



-- EXPORTS


elmPackageJson : Revision -> String
elmPackageJson revision =
    revision
        |> Revision.toDescription
        |> Description.encoder
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



-- TERMS


termsContent : TermsVersion -> RequestBuilder String
termsContent version =
    get (Constants.cdnBase ++ "/terms-of-service/" ++ TermsVersion.toString version ++ ".md")
        |> withExpect Http.expectString


acceptTerms : TermsVersion -> RequestBuilder Http.NoContent
acceptTerms termsVersion =
    post (fullUrl <| "/terms/" ++ TermsVersion.toString termsVersion ++ "/accept")
        |> withApiHeaders
        |> withExpect Http.expectNoContent



-- UPLOAD


uploadExpect : Expect ( UploadSignature, UploadSignature )
uploadExpect =
    Decode.map2 (,)
        (Decode.field "revision" UploadSignature.decoder)
        (Decode.field "result" UploadSignature.decoder)
        |> Http.expectJson


uploadSignatures : Revision -> Task ApiError ( UploadSignature, UploadSignature )
uploadSignatures revision =
    get (fullUrl "/upload")
        |> withExpect uploadExpect
        |> (\builder ->
                case ( revision.id, revision.owned ) of
                    ( Just { projectId, revisionNumber }, True ) ->
                        builder
                            |> withQueryParams [ ( "projectId", projectId ), ( "revisionNumber", toString (revisionNumber + 1) ) ]

                    _ ->
                        builder
           )
        |> toTask
