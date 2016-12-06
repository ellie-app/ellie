module Api
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
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Http exposing (Request, Expect)
import HttpBuilder exposing (..)
import RemoteData exposing (RemoteData)
import SemVer.VersionRange as VersionRange exposing (VersionRange)


-- TOP LEVEL


listOf : a -> List a
listOf a =
    [ a ]


send : (RemoteData Error a -> msg) -> RequestBuilder a -> Cmd msg
send tagger requestBuilder =
    requestBuilder
        |> toRequest
        |> Http.send (handleError >> tagger)



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
    post "http://localhost:1337/sessions"
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
    post ("http://localhost:1337/sessions/" ++ session.id ++ "/compile")
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
    put ("http://localhost:1337/sessions/" ++ session.id ++ "/dependencies")
        |> withHeader "Accept" "application/json"
        |> withJsonBody (addDependenciesPayload dependencies)
