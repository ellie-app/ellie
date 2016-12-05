module Api
    exposing
        ( Error(..)
        , handleError
        , Session
        , createSession
        , removeSession
        , Location
        , Region
        , CompileError
        , compile
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Http exposing (Request, Expect)
import HttpBuilder exposing (..)
import RemoteData exposing (RemoteData)
import SemVer.Version as Version exposing (Version)
import SemVer.VersionRange as VersionRange exposing (VersionRange)


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


type Session
    = Session String


createSessionExpect : Expect Session
createSessionExpect =
    Decode.field "id" Decode.string
        |> Decode.map Session
        |> Http.expectJson


createSession : Request Session
createSession =
    post "http://localhost:1337/session"
        |> withHeader "Content-Type" "application/json"
        |> withHeader "Accept" "application/json"
        |> withExpect createSessionExpect
        |> toRequest


removeSession : Session -> Request ()
removeSession (Session sessionId) =
    delete ("http://localhost:1337/session/" ++ sessionId)
        |> withHeader "Content-Type" "application/json"
        |> toRequest



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


compile : Session -> String -> Request (List CompileError)
compile (Session sessionId) source =
    post ("http://localhost:1337/session/" ++ sessionId ++ "/compile")
        |> withHeader "Accept" "application/json"
        |> withJsonBody (compilePayload source)
        |> withExpect compileExpect
        |> toRequest



-- DEPENDENCIES


type alias Dependency =
    { username : String
    , name : String
    , range : VersionRange
    }


addDependenciesPayload : List Dependency -> Value
addDependenciesPayload dependencies =
    dependencies
        |> List.map
