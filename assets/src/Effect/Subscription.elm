module Effect.Subscription exposing (..)

import Data.Jwt as Jwt exposing (Jwt)
import Graphqelm.Document as Document
import Graphqelm.Http
import Graphqelm.Operation exposing (RootSubscription)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)
import Keyboard exposing (KeyCode)


type Subscription msg
    = AbsintheSubscription String (SelectionSet msg RootSubscription) (Bool -> msg)
    | PortReceive String (Value -> msg)
    | KeyPress KeyCode msg
    | Batch (List (Subscription msg))
    | None


none : Subscription msg
none =
    None


batch : List (Subscription msg) -> Subscription msg
batch =
    Batch


eq : Subscription msg -> Subscription msg -> Bool
eq left right =
    case ( left, right ) of
        ( AbsintheSubscription lUrl lSelection _, AbsintheSubscription rUrl rSelection _ ) ->
            (lUrl == rUrl)
                && (Document.serializeSubscription lSelection == Document.serializeSubscription rSelection)

        ( PortReceive lChannel _, PortReceive rChannel _ ) ->
            lChannel == rChannel

        ( KeyPress lCode _, KeyPress rCode _ ) ->
            lCode == rCode

        ( Batch lSubs, Batch rSubs ) ->
            if List.length lSubs == List.length rSubs then
                List.all identity (List.map2 eq lSubs rSubs)
            else
                False

        ( None, None ) ->
            True

        _ ->
            False


map : (a -> b) -> Subscription a -> Subscription b
map f cmd =
    case cmd of
        AbsintheSubscription url selection onStatus ->
            AbsintheSubscription url (SelectionSet.map f selection) (onStatus >> f)

        PortReceive channel callback ->
            PortReceive channel (callback >> f)

        KeyPress code msg ->
            KeyPress code (f msg)

        Batch subs ->
            Batch (List.map (map f) subs)

        None ->
            None
