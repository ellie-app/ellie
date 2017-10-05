module Extra.Result exposing (..)


fold : (a -> b) -> (x -> b) -> Result x a -> b
fold success error result =
    case result of
        Ok a ->
            success a

        Err x ->
            error x
