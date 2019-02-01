module Effect.Subscription exposing (Subscription(..), batch, eq, map, none)

import Graphql.Document as Document
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)


type Subscription msg
    = AbsintheSubscription { url : String, token : Maybe String } (SelectionSet msg RootSubscription) (Bool -> msg)
    | PortReceive String (Value -> msg)
    | KeyPress Int msg
    | KeyCombo { meta : Bool, shift : Bool, key : String } msg
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
        ( AbsintheSubscription lConfig lSelection _, AbsintheSubscription rConfig rSelection _ ) ->
            (lConfig.token == rConfig.token)
                && (Document.serializeSubscription lSelection == Document.serializeSubscription rSelection)

        ( PortReceive lChannel _, PortReceive rChannel _ ) ->
            lChannel == rChannel

        ( KeyPress lCode _, KeyPress rCode _ ) ->
            lCode == rCode

        ( KeyCombo lCombo _, KeyCombo rCombo _ ) ->
            True

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
        AbsintheSubscription socketConfig selection onStatus ->
            AbsintheSubscription socketConfig (SelectionSet.map f selection) (onStatus >> f)

        PortReceive channel callback ->
            PortReceive channel (callback >> f)

        KeyPress code msg ->
            KeyPress code (f msg)

        KeyCombo combo msg ->
            KeyCombo combo (f msg)

        Batch subs ->
            Batch (List.map (map f) subs)

        None ->
            None
