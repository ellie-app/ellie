module Data.Extra.EveryDict exposing (..)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Data.Either exposing (Either(..))


encoder : (k -> Value) -> (v -> Value) -> EveryDict k v -> Value
encoder key value dict =
    dict
        |> EveryDict.toList
        |> List.map (\( k, v ) -> Encode.list [ key k, value v ])
        |> Encode.list


decoder : Decoder k -> Decoder v -> Decoder (EveryDict k v)
decoder key value =
    Decode.map2 (,)
        (Decode.index 0 key)
        (Decode.index 1 value)
        |> Decode.list
        |> Decode.map EveryDict.fromList


foldrWithKey : (k -> k -> Order) -> (k -> a -> b -> b) -> b -> EveryDict k a -> b
foldrWithKey cmp f z dict =
    dict
        |> EveryDict.toList
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> cmp k1 k2)
        |> List.foldr (uncurry f) z


getUnsafe : k -> EveryDict k v -> v
getUnsafe key dict =
    case EveryDict.get key dict of
        Just value ->
            value

        Nothing ->
            Debug.crash <| "Failed trying to get " ++ toString key ++ " from dict with keys " ++ toString (EveryDict.keys dict)


viewWithKey : EveryDict k v -> Maybe ( ( k, v ), EveryDict k v )
viewWithKey allDict =
    case EveryDict.toList allDict of
        ( k, v ) :: _ ->
            Just ( ( k, v ), EveryDict.remove k allDict )

        _ ->
            Nothing


mapKeys : (k -> kk) -> EveryDict k v -> EveryDict kk v
mapKeys f dict =
    dict
        |> EveryDict.toList
        |> List.map (\( k, v ) -> ( f k, v ))
        |> EveryDict.fromList


fromDict : Dict comparable v -> EveryDict comparable v
fromDict dict =
    dict
        |> Dict.toList
        |> EveryDict.fromList


mapEither : (a -> Either b c) -> EveryDict k a -> ( EveryDict k b, EveryDict k c )
mapEither f dict =
    mapEitherWithKey (\_ v -> f v) dict


mapEitherWithKey : (k -> a -> Either b c) -> EveryDict k a -> ( EveryDict k b, EveryDict k c )
mapEitherWithKey f dict =
    dict
        |> EveryDict.toList
        |> List.foldr
            (\( k, v ) ( l, r ) ->
                case f k v of
                    Left out ->
                        ( EveryDict.insert k out l, r )

                    Right out ->
                        ( l, EveryDict.insert k out r )
            )
            ( EveryDict.empty, EveryDict.empty )


insertWith : (v -> v -> v) -> k -> v -> EveryDict k v -> EveryDict k v
insertWith fn k v dict =
    EveryDict.update k (Maybe.map (fn v) >> Maybe.withDefault v >> Just) dict
