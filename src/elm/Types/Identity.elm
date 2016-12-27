module Types.Identity
    exposing
        ( Identity
        , map
        , create
        , fold
        )


type Identity a
    = Identity a


create : a -> Identity a
create a =
    Identity a


map : (a -> b) -> Identity a -> Identity b
map tagger (Identity a) =
    Identity <| tagger a


fold : (a -> b) -> Identity a -> b
fold tagger (Identity a) =
    tagger a
