module Data.Transition exposing (Transition(..), exit, fold, step)


type Transition a b
    = Step a
    | Exit b


step : a -> Transition a b
step =
    Step


exit : b -> Transition a b
exit =
    Exit


fold : (a -> c) -> (b -> c) -> Transition a b -> c
fold l r transition =
    case transition of
        Step a ->
            l a

        Exit b ->
            r b
