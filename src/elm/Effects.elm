effect module Effects
    where { subscription = MySub }
    exposing
        ( windowUnload
        )

import Time exposing (Time)
import Process
import Task exposing (Task)
import Json.Decode as Decode
import Dom.LowLevel as Dom


-- PUBLIC API


windowUnload : msg -> Sub msg
windowUnload msg =
    subscription <| OnWindowUnload msg



-- EFFECT MANAGER


type MyCmd msg
    = Debounce String Time msg


type MySub msg
    = OnWindowUnload msg


subMap : (a -> b) -> MySub a -> MySub b
subMap tagger sub =
    case sub of
        OnWindowUnload msg ->
            OnWindowUnload <| tagger msg


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap tagger cmd =
    case cmd of
        Debounce tag time msg ->
            Debounce tag time <| tagger msg


type alias Watcher msg =
    { handlers : List msg
    , pid : Process.Id
    }


type alias State msg =
    { onWindowUnload : Maybe (Watcher msg)
    }


type SelfMsg
    = WindowUnload


init : Task Never (State msg)
init =
    Task.succeed <|
        { onWindowUnload = Nothing
        }


onWindowUnloadEffects :
    Platform.Router msg SelfMsg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onWindowUnloadEffects router subs state =
    let
        nextOnWindowUnloadHandlers =
            List.filterMap
                (\sub ->
                    case sub of
                        OnWindowUnload msg ->
                            Just msg
                )
                subs
    in
        case ( state.onWindowUnload, nextOnWindowUnloadHandlers ) of
            ( Nothing, handler :: rest ) ->
                let
                    watcherTask =
                        Dom.onWindow "beforeunload"
                            (Decode.succeed WindowUnload)
                            (Platform.sendToSelf router)
                in
                    Process.spawn watcherTask
                        |> Task.andThen (\pid -> Task.succeed <| { state | onWindowUnload = Just <| Watcher nextOnWindowUnloadHandlers pid })

            ( Just watcher, [] ) ->
                Process.kill watcher.pid
                    |> Task.andThen (\_ -> Task.succeed <| { state | onWindowUnload = Nothing })

            ( Just watcher, handlers ) ->
                Task.succeed <| { state | onWindowUnload = Just <| { watcher | handlers = handlers } }

            ( Nothing, [] ) ->
                Task.succeed state


onWindowUnloadSelfMsg :
    Platform.Router msg SelfMsg
    -> State msg
    -> Task Never (State msg)
onWindowUnloadSelfMsg router state =
    case state.onWindowUnload of
        Just watcher ->
            watcher.handlers
                |> List.map (Platform.sendToApp router)
                |> Task.sequence
                |> Task.andThen (\_ -> Task.succeed state)

        Nothing ->
            Task.succeed state


onEffects :
    Platform.Router msg SelfMsg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router subs state =
    onWindowUnloadEffects router subs state


onSelfMsg :
    Platform.Router msg SelfMsg
    -> SelfMsg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        WindowUnload ->
            onWindowUnloadSelfMsg router state
