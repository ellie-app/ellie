module Effect.Command exposing (..)

import Data.Jwt as Jwt exposing (Jwt)
import Graphqelm.Document as Document
import Graphqelm.Http
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)


type Command msg
    = GraphqlQuery String (Maybe Jwt) (SelectionSet msg RootQuery) (Graphqelm.Http.Error () -> msg)
    | GraphqlMutation String (Maybe Jwt) (SelectionSet msg RootMutation) (Graphqelm.Http.Error () -> msg)
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
        ( GraphqlQuery lUrl lToken lSelection _, GraphqlQuery rUrl rToken rSelection _ ) ->
            (lUrl == rUrl)
                && (Document.serializeQuery lSelection == Document.serializeQuery rSelection)
                && (lToken == rToken)

        ( GraphqlMutation lUrl lToken lSelection _, GraphqlMutation rUrl rToken rSelection _ ) ->
            (lUrl == rUrl)
                && (Document.serializeMutation lSelection == Document.serializeMutation rSelection)
                && (lToken == rToken)

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
        GraphqlQuery url token selection onError ->
            GraphqlQuery url token (SelectionSet.map f selection) (onError >> f)

        GraphqlMutation url token selection onError ->
            GraphqlMutation url token (SelectionSet.map f selection) (onError >> f)

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
