module Extra.Result exposing (..)


fold : (a -> b) -> (x -> b) -> Result x a -> b
fold success error result =
    case result of
        Ok a ->
            success a

        Err x ->
            error x


traverse : (a -> Result x b) -> List a -> Result x (List b)
traverse toResult list =
    List.foldr
        (\next result -> Result.map2 (::) (toResult next) result)
        (Ok [])
        list
