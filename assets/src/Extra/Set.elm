module Extra.Set exposing (superset)

import Set exposing (Set)


superset : Set comparable -> Set comparable -> Bool
superset child parent =
    (Set.size child <= Set.size parent)
        && Set.foldl (\x acc -> acc && Set.member x parent) True child
