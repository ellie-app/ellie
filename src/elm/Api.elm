module Api
    exposing
        ( Error(..)
        , handleError
        , Session
        , createSession
        , removeSession
        , compile
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Http exposing (Request, Expect)
import LoadState exposing (LoadState)
import HttpBuilder exposing (..)


-- ERRORS


type Error
    = Badness


upgradeError : Http.Error -> Error
upgradeError error =
    Badness


handleError : Result Http.Error a -> LoadState Error a
handleError result =
    result
        |> Result.mapError upgradeError
        |> LoadState.fromResult



-- API


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


compilePayload : String -> Value
compilePayload source =
    Encode.object
        [ ( "source", Encode.string source )
        ]


compileExpect : Expect String
compileExpect =
    Decode.field "result" Decode.string
        |> Http.expectJson


compile : Session -> String -> Request String
compile (Session sessionId) source =
    post ("http://localhost:1337/session/" ++ sessionId ++ "/compile")
        |> withHeader "Accept" "application/json"
        |> withJsonBody (compilePayload source)
        |> withExpect compileExpect
        |> toRequest
