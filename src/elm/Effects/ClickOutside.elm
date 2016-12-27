effect module Effects.ClickOutside where { subscription = MySub } exposing (clickOutside)

import Process
import Task exposing (Task)
import Dict exposing (Dict)
import Native.ClickOutside


clickOutside : String -> msg -> Sub msg
clickOutside id msg =
    subscription <| ClickOutside id msg


type MySub msg
    = ClickOutside String msg


subMap : (a -> b) -> MySub a -> MySub b
subMap tagger sub =
    case sub of
        ClickOutside id msg ->
            ClickOutside id <| tagger msg


type alias State msg =
    { subs : List (MySub msg)
    , pid : Maybe Process.Id
    }


groupList : List ( String, a ) -> List ( String, List a )
groupList list =
    list
        |> List.foldl
            (\( key, value ) dict ->
                Dict.update
                    key
                    (\maybeStuff ->
                        case maybeStuff of
                            Just stuff ->
                                Just <| value :: stuff

                            Nothing ->
                                Just <| [ value ]
                    )
                    dict
            )
            Dict.empty
        |> Dict.toList


categorizeSubs : Platform.Router msg (SelfMsg msg) -> List (MySub msg) -> List ( String, Task Never () )
categorizeSubs router subs =
    subs
        |> List.map (\(ClickOutside id sub) -> ( id, sub ))
        |> groupList
        |> List.map
            (\( id, msgs ) ->
                ( id
                , msgs
                    |> List.map SelfMsg
                    |> List.map (Platform.sendToSelf router)
                    |> Task.sequence
                    |> Task.map (\_ -> ())
                )
            )


type SelfMsg msg
    = SelfMsg msg


init : Task Never (State msg)
init =
    Task.succeed
        { subs = []
        , pid = Nothing
        }


onEffects :
    Platform.Router msg (SelfMsg msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router subs currentState =
    case ( subs, currentState.pid ) of
        ( [], Just pid ) ->
            Process.kill pid
                |> Task.andThen (\_ -> Task.succeed <| State [] Nothing)

        ( [], Nothing ) ->
            Task.succeed currentState

        ( _, _ ) ->
            if subs /= currentState.subs then
                currentState.pid
                    |> Maybe.map Process.kill
                    |> Maybe.withDefault (Task.succeed ())
                    |> Task.andThen (\_ -> Task.succeed (categorizeSubs router subs))
                    |> Task.andThen (\premap -> Process.spawn <| Native.ClickOutside.onClickOutside premap)
                    |> Task.andThen (\newPid -> Task.succeed <| State subs (Just newPid))
            else
                Task.succeed currentState


onSelfMsg :
    Platform.Router msg (SelfMsg msg)
    -> SelfMsg msg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router selfMsg currentState =
    case selfMsg of
        SelfMsg msg ->
            Platform.sendToApp router msg
                |> Task.andThen (\_ -> Task.succeed currentState)
