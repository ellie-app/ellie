module App.Model
    exposing
        ( Model
        , model
        , updateClientRevision
        , isOwnedProject
        , isSavedProject
        , isRevisionChanged
        )

import RemoteData exposing (RemoteData(..))
import Types.ApiError as ApiError exposing (ApiError)
import Types.Session as Session exposing (Session)
import Types.Revision as Revision exposing (Revision)
import Types.CompileError as CompileError exposing (CompileError)
import App.Routing as Routing exposing (Route(..))


type alias Model =
    { session : RemoteData ApiError Session
    , serverRevision : RemoteData ApiError Revision
    , clientRevision : Revision
    , currentRoute : Route
    , compileResult : RemoteData ApiError (List CompileError)
    , elmCodeChanged : Bool
    , firstCompileComplete : Bool
    , saveState : RemoteData ApiError ()
    , isOnline : Bool
    }


model : Model
model =
    { session = NotAsked
    , serverRevision = NotAsked
    , clientRevision = Revision.empty
    , currentRoute = NotFound
    , compileResult = NotAsked
    , elmCodeChanged = False
    , firstCompileComplete = False
    , saveState = NotAsked
    , isOnline = False
    }


isRevisionChanged : Model -> Bool
isRevisionChanged model =
    model.serverRevision
        |> RemoteData.map ((/=) model.clientRevision)
        |> RemoteData.withDefault False


isSavedProject : Model -> Bool
isSavedProject model =
    model.serverRevision
        |> RemoteData.toMaybe
        |> Maybe.andThen .projectId
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


isOwnedProject : Model -> Bool
isOwnedProject model =
    model.serverRevision
        |> RemoteData.toMaybe
        |> Maybe.map .owned
        |> Maybe.withDefault False


updateClientRevision : (Revision -> Revision) -> Model -> Model
updateClientRevision updater model =
    { model | clientRevision = updater model.clientRevision }
