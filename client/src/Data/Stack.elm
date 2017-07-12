module Data.Stack exposing (Stack, empty, push, pop, toList)


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


push : a -> Stack a -> Stack a
push item (Stack list) =
    Stack (item :: list)


pop : Stack a -> ( Maybe a, Stack a )
pop (Stack list) =
    case list of
        head :: tail ->
            ( Just head, Stack tail )

        _ ->
            ( Nothing, empty )


toList : Stack a -> List a
toList (Stack list) =
    list
