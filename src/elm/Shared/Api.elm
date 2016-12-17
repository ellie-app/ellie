module Shared.Api
    exposing
        ( Error(..)
        , send
        , searchPackages
        , searchVersions
        , createSession
        , removeSession
        , compile
        , addDependencies
        , latestRevision
        , exactRevision
        , createProjectFromRevision
        , createRevision
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Http exposing (Request, Expect)
import HttpBuilder exposing (..)
import RemoteData exposing (RemoteData)
import Types.Version as Version exposing (Version)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Uuid as Uuid exposing (Uuid)
import Types.Session as Session exposing (Session)
import Types.Dependency as Dependency exposing (Dependency)
import Types.CompileError as CompileError exposing (CompileError)
import Types.NewRevision as NewRevision exposing (NewRevision)
import Types.ExistingRevision as ExistingRevision exposing (ExistingRevision)
import Shared.Constants as Constants


-- TOP LEVEL


listOf : a -> List a
listOf a =
    [ a ]


send : (RemoteData Error a -> msg) -> RequestBuilder a -> Cmd msg
send tagger requestBuilder =
    requestBuilder
        |> toRequest
        |> Http.send (handleError >> tagger)


fullUrl : String -> String
fullUrl path =
    Constants.apiBase ++ path



-- ERRORS


type Error
    = Badness


upgradeError : Http.Error -> Error
upgradeError error =
    Badness


handleError : Result Http.Error a -> RemoteData Error a
handleError result =
    result
        |> Result.mapError upgradeError
        |> RemoteData.fromResult



-- SEARCH


searchPackages : String -> RequestBuilder (List PackageSearchResult)
searchPackages searchTerm =
    get (fullUrl "/packages/search")
        |> withQueryParams [ ( "q", searchTerm ) ]
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson (Decode.list PackageSearchResult.decode))


searchVersions : String -> String -> String -> RequestBuilder (List Version)
searchVersions username packageName searchTerm =
    get (fullUrl ("/packages/" ++ username ++ "/" ++ packageName ++ "/versions/search"))
        |> withQueryParams [ ( "q", searchTerm ) ]
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson (Decode.list Version.decode))



-- SESSIONS


createSession : RequestBuilder Session
createSession =
    post (fullUrl "/sessions")
        |> withHeader "Content-Type" "application/json"
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson Session.decode)


removeSession : Session -> RequestBuilder ()
removeSession session =
    delete ("http://localhost:1337/sessions/" ++ session.id)
        |> withHeader "Content-Type" "application/json"



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


compile : Session -> String -> RequestBuilder (List CompileError)
compile session source =
    post (fullUrl ("/sessions/" ++ session.id ++ "/compile"))
        |> withHeader "Accept" "application/json"
        |> withJsonBody (compilePayload source)
        |> withExpect compileExpect



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
        |> withHeader "Accept" "application/json"
        |> withJsonBody (addDependenciesPayload dependencies)



-- REVISIONS AND PROJECTS


latestRevision : Uuid -> RequestBuilder ExistingRevision
latestRevision projectId =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/latest"))
        |> withHeader "Accept" "appication/json"
        |> withExpect (Http.expectJson ExistingRevision.decode)


exactRevision : Uuid -> Int -> RequestBuilder ExistingRevision
exactRevision projectId revisionNumber =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/" ++ toString revisionNumber))
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson ExistingRevision.decode)


createProjectFromRevision : NewRevision -> RequestBuilder ExistingRevision
createProjectFromRevision revision =
    post (fullUrl "/projects")
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson ExistingRevision.decode)
        |> withJsonBody (NewRevision.encode revision)


createRevision : ExistingRevision -> RequestBuilder ()
createRevision revision =
    put (fullUrl "/projects" ++ Uuid.toString revision.projectId ++ "/revisions")
        |> withHeader "Accept" "application/json"
        |> withJsonBody (ExistingRevision.encode revision)
