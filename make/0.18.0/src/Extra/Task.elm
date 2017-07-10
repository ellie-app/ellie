module Extra.Task exposing (..)

import Task exposing (Task)


foldl : {- (Foldable t, Monad m) => -} (a -> b -> Task x b) -> b -> List a -> Task x b
foldl f z0 xs =
    let
        f_ x k z =
            f x z |> Task.andThen k
    in
    List.foldr f_ Task.succeed xs z0


fromResult : Result x a -> Task x a
fromResult result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x
