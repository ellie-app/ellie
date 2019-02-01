module Data.Group exposing
    ( Group
    , batch
    , none
    , one
    )


type Group a
    = None
    | Single a
    | Batch (List (Group a))


none : Group a
none =
    None


one : a -> Group a
one =
    Single


batch : List (Group a) -> Group a
batch =
    Batch


toList : Group a -> List a
toList group =
    toListHelp [] group


toListHelp : List a -> Group a -> List a
toListHelp outputs group =
    case group of
        None ->
            outputs

        Single value ->
            value :: outputs

        Batch many ->
            List.concatMap (toListHelp []) many ++ outputs
