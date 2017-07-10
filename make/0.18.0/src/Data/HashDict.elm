module Data.HashDict
    exposing
        ( HashDict
        , decoder
        , diff
        , empty
        , encoder
        , filter
        , foldl
        , foldr
        , foldrWithKey
        , fromDict
        , fromList
        , get
        , getUnsafe
        , insert
        , insertWith
        , intersect
        , isEmpty
        , keys
        , map
        , mapEither
        , mapEitherWithKey
        , mapKeys
        , member
        , merge
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        , update
        , values
        , viewWithKey
        )

import Data.Either exposing (Either(..))
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Native.Hash


hash : a -> Int
hash =
    Native.Hash.hash


type HashDict k v
    = HashDict (IntDict ( k, v ))


fromList : List ( k, v ) -> HashDict k v
fromList stuff =
    HashDict
        (stuff |> List.map (\( k, v ) -> ( hash k, ( k, v ) )) |> IntDict.fromList)


empty : HashDict k v
empty =
    HashDict IntDict.empty


singleton : k -> v -> HashDict k v
singleton k v =
    fromList [ ( k, v ) ]


insert : k -> v -> HashDict k v -> HashDict k v
insert k v (HashDict data) =
    HashDict <| IntDict.insert (hash k) ( k, v ) data


update :
    k
    -> (Maybe v -> Maybe v)
    -> HashDict k v
    -> HashDict k v
update k fn (HashDict data) =
    HashDict <|
        IntDict.update
            (hash k)
            (Maybe.map Tuple.second >> fn >> Maybe.map ((,) k))
            data


remove : k -> HashDict k v -> HashDict k v
remove k (HashDict data) =
    HashDict (IntDict.remove (hash k) data)


isEmpty : HashDict k v -> Bool
isEmpty (HashDict data) =
    IntDict.isEmpty data


member : k -> HashDict k v -> Bool
member k (HashDict data) =
    IntDict.member (hash k) data


get : k -> HashDict k v -> Maybe v
get k (HashDict data) =
    data
        |> IntDict.get (hash k)
        |> Maybe.map Tuple.second


size : HashDict k v -> Int
size (HashDict data) =
    IntDict.size data


keys : HashDict k v -> List k
keys (HashDict data) =
    data
        |> IntDict.values
        |> List.map Tuple.first


values : HashDict k v -> List v
values (HashDict data) =
    data
        |> IntDict.values
        |> List.map Tuple.second


toList : HashDict k v -> List ( k, v )
toList (HashDict data) =
    IntDict.values data


map : (k -> v1 -> v2) -> HashDict k v1 -> HashDict k v2
map mapper (HashDict data) =
    HashDict <|
        IntDict.map
            (\_ ( k, v ) -> ( k, mapper k v ))
            data


foldl :
    (k -> v -> b -> b)
    -> b
    -> HashDict k v
    -> b
foldl folder init (HashDict data) =
    IntDict.foldl
        (\_ ( k, v ) b -> folder k v b)
        init
        data


foldr :
    (k -> v -> b -> b)
    -> b
    -> HashDict k v
    -> b
foldr folder init (HashDict data) =
    IntDict.foldr
        (\_ ( k, v ) b -> folder k v b)
        init
        data


filter : (k -> v -> Bool) -> HashDict k v -> HashDict k v
filter filterer (HashDict data) =
    HashDict <|
        IntDict.filter
            (\i ( k, v ) -> filterer k v)
            data


partition :
    (k -> v -> Bool)
    -> HashDict k v
    -> ( HashDict k v, HashDict k v )
partition partitioner (HashDict data) =
    let
        ( l, r ) =
            IntDict.partition
                (\_ ( k, v ) -> partitioner k v)
                data
    in
    ( HashDict l, HashDict r )


union : HashDict k v -> HashDict k v -> HashDict k v
union (HashDict ldata) (HashDict rdata) =
    HashDict (IntDict.union ldata rdata)


intersect : HashDict k v -> HashDict k v -> HashDict k v
intersect (HashDict ldata) (HashDict rdata) =
    HashDict (IntDict.intersect ldata rdata)


diff : HashDict k v -> HashDict k v -> HashDict k v
diff (HashDict ldata) (HashDict rdata) =
    HashDict (IntDict.diff ldata rdata)


merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> HashDict k a
    -> HashDict k b
    -> result
    -> result
merge inLeft inBoth inRight (HashDict ldata) (HashDict rdata) init =
    IntDict.merge
        (\_ ( k, v ) r -> inLeft k v r)
        (\_ ( kl, vl ) ( kr, vr ) r -> inBoth kl vl vr r)
        (\_ ( k, v ) r -> inRight k v r)
        ldata
        rdata
        init


encoder : (k -> Value) -> (v -> Value) -> HashDict k v -> Value
encoder key value (HashDict data) =
    data
        |> IntDict.toList
        |> List.map (\( i, ( k, v ) ) -> Encode.list [ Encode.int i, Encode.list [ key k, value v ] ])
        |> Encode.list


decoder : Decoder k -> Decoder v -> Decoder (HashDict k v)
decoder key value =
    Decode.map2 (,)
        (Decode.index 0 Decode.int)
        (Decode.index 1
            (Decode.map2 (,)
                (Decode.index 0 key)
                (Decode.index 1 value)
            )
        )
        |> Decode.list
        |> Decode.map (IntDict.fromList >> HashDict)


foldrWithKey : (k -> k -> Order) -> (k -> a -> b -> b) -> b -> HashDict k a -> b
foldrWithKey cmp f z dict =
    dict
        |> toList
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> cmp k1 k2)
        |> List.foldr (uncurry f) z


getUnsafe : k -> HashDict k v -> v
getUnsafe key dict =
    case get key dict of
        Just value ->
            value

        Nothing ->
            Debug.crash <| "Failed trying to get " ++ toString key ++ " from dict with keys " ++ toString (keys dict)


viewWithKey : HashDict k v -> Maybe ( ( k, v ), HashDict k v )
viewWithKey hashDict =
    case toList hashDict of
        ( k, v ) :: _ ->
            Just ( ( k, v ), remove k hashDict )

        _ ->
            Nothing


mapKeys : (k -> kk) -> HashDict k v -> HashDict kk v
mapKeys f hashDict =
    hashDict
        |> toList
        |> List.map (\( k, v ) -> ( f k, v ))
        |> fromList


fromDict : Dict comparable v -> HashDict comparable v
fromDict dict =
    dict
        |> Dict.toList
        |> fromList


mapEither : (a -> Either b c) -> HashDict k a -> ( HashDict k b, HashDict k c )
mapEither f dict =
    mapEitherWithKey (\_ v -> f v) dict


mapEitherWithKey : (k -> a -> Either b c) -> HashDict k a -> ( HashDict k b, HashDict k c )
mapEitherWithKey f ((HashDict _) as dict) =
    dict
        |> toList
        |> List.foldr
            (\( k, v ) ( l, r ) ->
                case f k v of
                    Left out ->
                        ( insert k out l, r )

                    Right out ->
                        ( l, insert k out r )
            )
            ( empty, empty )


insertWith : (v -> v -> v) -> k -> v -> HashDict k v -> HashDict k v
insertWith fn k v dict =
    update k (Maybe.map (fn v) >> Maybe.withDefault v >> Just) dict
