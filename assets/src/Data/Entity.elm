module Data.Entity exposing
    ( Entity(..)
    , decoder
    , encoder
    , key
    , map
    , record
    )

{-| A representation of some data that is stored based on some unique ID.
This is a much nicer way to represent this situation than by putting an
`id` field on a record and making it a `Maybe Id`. Implementing this module
in Elm seems like mostly useless boilerplate. The reason we do it is to
correctly mirror the same type in our PureScript backend. In PureScript this
type is much more useful because we can constrain it with a typeclass called
IdentifiedBy that ensures a `k` is meant to represent a `v`. We can write
`instance identifiedByRevisionIdRevision :: IdentifiedBy Revision.Id Revision`
and then as long as that is the only instance of the typeclass we can never
make the mistake of constructing an entity where a Revision is identified by
something other than a Revision.Id. This is pretty cool! We can't do that in
Elm but it's still nice to have parity of representation between the frontend
and the backend. Also, regardless of the degree of type safety, this is still
a much nicer way to represent IDs than `{ a | id : Maybe Id }`.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Entity k v
    = Entity k v


key : Entity k v -> k
key (Entity k _) =
    k


record : Entity k v -> v
record (Entity _ v) =
    v


map : (a -> b) -> Entity k a -> Entity k b
map update (Entity k v) =
    Entity k (update v)


decoder : Decoder k -> Decoder v -> Decoder (Entity k v)
decoder key value =
    Decode.map2 Entity
        (Decode.field "key" key)
        (Decode.field "record" value)


encoder : (k -> Value) -> (v -> Value) -> Entity k v -> Value
encoder key value (Entity k v) =
    Encode.object
        [ ( "key", key k )
        , ( "record", value v )
        ]
