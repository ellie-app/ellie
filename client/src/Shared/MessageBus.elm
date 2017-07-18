effect module Shared.MessageBus
    where { command = BusCmd, subscription = BusSub }
    exposing
        ( notifications
        , notify
        )

import Data.Ellie.Notification as Notification exposing (Notification)
import Date
import Task exposing (Task)
import Time


notifications : (Notification -> msg) -> Sub msg
notifications tagger =
    subscription <| Notifications tagger


notify : Notification.Level -> String -> String -> Cmd msg
notify level title message =
    command <| Notify level title message


type BusCmd msg
    = Notify Notification.Level String String


type BusSub msg
    = Notifications (Notification -> msg)


type SelfMsg
    = Notified Notification


cmdMap : (a -> b) -> BusCmd a -> BusCmd b
cmdMap tagger (Notify level title message) =
    Notify level title message


subMap : (a -> b) -> BusSub a -> BusSub b
subMap outerTagger (Notifications innerTagger) =
    Notifications (outerTagger << innerTagger)


type alias State msg =
    List (Notification -> msg)


init : Task Never (State msg)
init =
    Task.succeed []


unwrapSubs : List (BusSub msg) -> State msg
unwrapSubs subs =
    List.map (\(Notifications tagger) -> tagger) subs


stampAndSend : Platform.Router msg SelfMsg -> Notification.Level -> String -> String -> Task Never ()
stampAndSend router level title message =
    Time.now
        |> Task.map (Date.fromTime >> Notification level message title >> Notified)
        |> Task.andThen (Platform.sendToSelf router)


onEffects :
    Platform.Router msg SelfMsg
    -> List (BusCmd msg)
    -> List (BusSub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs state =
    case ( subs, cmds ) of
        ( s :: _, c :: _ ) ->
            cmds
                |> List.map (\(Notify level title message) -> stampAndSend router level title message)
                |> Task.sequence
                |> Task.andThen (\_ -> Task.succeed <| unwrapSubs subs)

        ( _, _ ) ->
            Task.succeed <| unwrapSubs subs


onSelfMsg :
    Platform.Router msg SelfMsg
    -> SelfMsg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Notified notification ->
            state
                |> List.map (\tagger -> Platform.sendToApp router (tagger notification))
                |> Task.sequence
                |> Task.andThen (\_ -> Task.succeed state)
