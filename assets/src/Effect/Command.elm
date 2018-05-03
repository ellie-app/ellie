module Effect.Command exposing (..)

import Data.Jwt as Jwt exposing (Jwt)
import Graphqelm.Document as Document
import Graphqelm.Http
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)


type CacheLevel
    = Temporary
    | Permanent
    | AlwaysFetch


type Command msg
    = GraphqlQuery
        { url : String
        , token : Maybe Jwt
        , selection : SelectionSet msg RootQuery
        , onError : Graphqelm.Http.Error () -> msg
        , debounce : Maybe String
        , cache : CacheLevel
        }
    | GraphqlMutation
        { url : String
        , token : Maybe Jwt
        , selection : SelectionSet msg RootMutation
        , onError : Graphqelm.Http.Error () -> msg
        , debounce : Maybe String
        }
    | PortSend String Value
    | NewUrl String
    | Redirect String
    | Delay Int msg
    | ReloadOutput
    | Batch (List (Command msg))
    | None


none : Command msg
none =
    None


batch : List (Command msg) -> Command msg
batch =
    Batch


eq : Command msg -> Command msg -> Bool
eq left right =
    case ( left, right ) of
        ( GraphqlQuery l, GraphqlQuery r ) ->
            (l.url == r.url)
                && (Document.serializeQuery l.selection == Document.serializeQuery r.selection)
                && (l.token == r.token)
                && (l.debounce == r.debounce)
                && (l.cache == r.cache)

        ( GraphqlMutation l, GraphqlMutation r ) ->
            (l.url == r.url)
                && (Document.serializeMutation l.selection == Document.serializeMutation r.selection)
                && (l.token == r.token)
                && (l.debounce == r.debounce)

        ( PortSend lChannel lData, PortSend rChannel rData ) ->
            (lChannel == rChannel) && (lData == rData)

        ( NewUrl lUrl, NewUrl rUrl ) ->
            lUrl == rUrl

        ( Redirect lUrl, Redirect rUrl ) ->
            lUrl == rUrl

        ( Delay lMillis _, Delay rMillis _ ) ->
            lMillis == rMillis

        ( ReloadOutput, ReloadOutput ) ->
            True

        ( Batch lCmds, Batch rCmds ) ->
            (List.length lCmds == List.length rCmds)
                && List.all identity (List.map2 eq lCmds rCmds)

        ( None, None ) ->
            True

        _ ->
            False


map : (a -> b) -> Command a -> Command b
map f cmd =
    case cmd of
        GraphqlQuery stuff ->
            GraphqlQuery
                { url = stuff.url
                , token = stuff.token
                , debounce = stuff.debounce
                , cache = stuff.cache
                , selection = SelectionSet.map f stuff.selection
                , onError = stuff.onError >> f
                }

        GraphqlMutation stuff ->
            GraphqlMutation
                { url = stuff.url
                , token = stuff.token
                , debounce = stuff.debounce
                , selection = SelectionSet.map f stuff.selection
                , onError = stuff.onError >> f
                }

        PortSend channel data ->
            PortSend channel data

        NewUrl url ->
            NewUrl url

        Redirect url ->
            Redirect url

        ReloadOutput ->
            ReloadOutput

        Delay millis msg ->
            Delay millis (f msg)

        Batch cmds ->
            Batch <| List.map (map f) cmds

        None ->
            None
