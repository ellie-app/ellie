module Shared.Api
    exposing
        ( Error(..)
        , send
        , Session
        , createSession
        , removeSession
        , Location
        , Region
        , CompileError
        , compile
        , Dependency
        , addDependencies
        , NewRevision
        , ExistingRevision
        , latestRevision
        , exactRevision
        , createProjectFromRevision
        , createRevision
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Http exposing (Request, Expect)
import HttpBuilder exposing (..)
import RemoteData exposing (RemoteData)
import Shared.SemVer.VersionRange as VersionRange exposing (VersionRange)
import Shared.Uuid as Uuid exposing (Uuid)
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



-- SESSIONS


type alias Session =
    { id : String
    }


createSessionExpect : Expect Session
createSessionExpect =
    Decode.succeed Session
        |> Decode.required "id" Decode.string
        |> Http.expectJson


createSession : RequestBuilder Session
createSession =
    post (fullUrl "/sessions")
        |> withHeader "Content-Type" "application/json"
        |> withHeader "Accept" "application/json"
        |> withExpect createSessionExpect


removeSession : Session -> RequestBuilder ()
removeSession session =
    delete ("http://localhost:1337/sessions/" ++ session.id)
        |> withHeader "Content-Type" "application/json"



-- COMPILATION


type alias Location =
    { line : Int
    , column : Int
    }


type alias Region =
    { start : Location
    , end : Location
    }


type alias CompileError =
    { tag : String
    , overview : String
    , details : String
    , subregion : Maybe Region
    , region : Region
    , level : String
    }


decodeLocation : Decoder Location
decodeLocation =
    Decode.succeed Location
        |> Decode.required "line" Decode.int
        |> Decode.required "column" Decode.int


decodeRegion : Decoder Region
decodeRegion =
    Decode.succeed Region
        |> Decode.required "start" decodeLocation
        |> Decode.required "end" decodeLocation


decodeCompileError : Decoder CompileError
decodeCompileError =
    Decode.succeed CompileError
        |> Decode.required "tag" Decode.string
        |> Decode.required "overview" Decode.string
        |> Decode.required "details" Decode.string
        |> Decode.required "subregion" (Decode.nullable decodeRegion)
        |> Decode.required "region" decodeRegion
        |> Decode.required "type" Decode.string


compilePayload : String -> Value
compilePayload source =
    Encode.object
        [ ( "source", Encode.string source )
        ]


compileExpect : Expect (List CompileError)
compileExpect =
    Decode.list decodeCompileError
        |> Http.expectJson


compile : Session -> String -> RequestBuilder (List CompileError)
compile session source =
    post (fullUrl ("/sessions/" ++ session.id ++ "/compile"))
        |> withHeader "Accept" "application/json"
        |> withJsonBody (compilePayload source)
        |> withExpect compileExpect



-- DEPENDENCIES


type alias Dependency =
    { username : String
    , name : String
    , range : VersionRange
    }


encodeDependency : Dependency -> Value
encodeDependency dependency =
    Encode.object
        [ ( "username", Encode.string dependency.username )
        , ( "name", Encode.string dependency.name )
        , ( "range", VersionRange.encode dependency.range )
        ]


decodeDependency : Decoder Dependency
decodeDependency =
    Decode.decode Dependency
        |> Decode.required "username" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "range" VersionRange.decode


addDependenciesPayload : List Dependency -> Value
addDependenciesPayload dependencies =
    dependencies
        |> List.map encodeDependency
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


type alias NewRevision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    }


type alias ExistingRevision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    , revisionNumber : Int
    , projectId : Uuid
    }


decodeExistingRevision : Decoder ExistingRevision
decodeExistingRevision =
    Decode.decode ExistingRevision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "dependencies" (Decode.list decodeDependency)
        |> Decode.required "revisionNumber" Decode.int
        |> Decode.required "projectId" Uuid.decode


encodeNewRevision : NewRevision -> Value
encodeNewRevision revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map encodeDependency revision.dependencies )
        ]


encodeExistingRevision : ExistingRevision -> Value
encodeExistingRevision revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map encodeDependency revision.dependencies )
        , ( "revisionNumber", Encode.int revision.revisionNumber )
        , ( "projectId", Uuid.encode revision.projectId )
        ]


latestRevision : Uuid -> RequestBuilder ExistingRevision
latestRevision projectId =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/latest"))
        |> withHeader "Accept" "appication/json"
        |> withExpect (Http.expectJson decodeExistingRevision)


exactRevision : Uuid -> Int -> RequestBuilder ExistingRevision
exactRevision projectId revisionNumber =
    get (fullUrl ("/projects/" ++ Uuid.toString projectId ++ "/revisions/" ++ toString revisionNumber))
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson decodeExistingRevision)


createProjectFromRevision : NewRevision -> RequestBuilder ExistingRevision
createProjectFromRevision revision =
    post (fullUrl "/projects")
        |> withHeader "Accept" "application/json"
        |> withExpect (Http.expectJson decodeExistingRevision)
        |> withJsonBody (encodeNewRevision revision)


createRevision : ExistingRevision -> RequestBuilder ()
createRevision revision =
    put (fullUrl "/projects" ++ Uuid.toString revision.projectId ++ "/revisions")
        |> withHeader "Accept" "application/json"
        |> withJsonBody (encodeExistingRevision revision)
