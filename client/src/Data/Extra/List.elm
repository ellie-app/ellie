module Data.Extra.List exposing (..)


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            if pred next then
                Just next
            else
                find pred rest


inits : List a -> List (List a)
inits =
    List.foldr (\e acc -> [] :: List.map ((::) e) acc) [ [] ]


foldl1 : (a -> a -> a) -> List a -> Maybe a
foldl1 f xs =
    let
        mf x m =
            Just
                (case m of
                    Nothing ->
                        x

                    Just y ->
                        f y x
                )
    in
        List.foldl mf Nothing xs


mapAccumR : (x -> acc -> ( acc, y )) -> acc -> List x -> ( acc, List y )
mapAccumR f init list =
    List.foldr
        (\x ( acc1, ys ) ->
            let
                ( acc2, y ) =
                    f x acc1
            in
                ( acc2, y :: ys )
        )
        ( init, [] )
        list
