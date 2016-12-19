effect module MessageBus
    where { command = BusCmd, subscription = BusSub }
    exposing
        ( BusMsg(..)
        , send
        , listen
        )

import Task exposing (Task)


send : BusMsg -> Cmd msg
send busMsg =
    command <| Send busMsg


listen : (BusMsg -> msg) -> Sub msg
listen tagger =
    subscription <| Listen tagger


type BusMsg
    = NoOp


type BusCmd msg
    = Send BusMsg


type BusSub msg
    = Listen (BusMsg -> msg)


cmdMap : (a -> b) -> BusCmd a -> BusCmd b
cmdMap tagger (Send busMsg) =
    Send busMsg


subMap : (a -> b) -> BusSub a -> BusSub b
subMap outerTagger (Listen innerTagger) =
    Listen (outerTagger << innerTagger)


type alias State msg =
    List (BusMsg -> msg)


init : Task Never (State msg)
init =
    Task.succeed []


unwrapSubs : List (BusSub msg) -> State msg
unwrapSubs subs =
    List.map (\(Listen tagger) -> tagger) subs


onEffects :
    Platform.Router msg BusMsg
    -> List (BusCmd msg)
    -> List (BusSub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs state =
    case ( subs, cmds ) of
        ( s :: _, c :: _ ) ->
            cmds
                |> List.map (\(Send busMsg) -> Platform.sendToSelf router busMsg)
                |> Task.sequence
                |> Task.andThen (\_ -> Task.succeed <| unwrapSubs subs)

        ( _, _ ) ->
            Task.succeed <| unwrapSubs subs


onSelfMsg :
    Platform.Router msg BusMsg
    -> BusMsg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router busMsg state =
    state
        |> List.map (\tagger -> Platform.sendToApp router (tagger busMsg))
        |> Task.sequence
        |> Task.andThen (\_ -> Task.succeed state)
