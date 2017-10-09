module Data.List.Iterator exposing (..)


type Iterator a
    = Iterator (List a) (Maybe a) (List a)


fromList : List a -> Iterator a
fromList list =
    Iterator [] Nothing list


before : Iterator a -> List a
before (Iterator xs _ _) =
    List.reverse xs


current : Iterator a -> Maybe a
current (Iterator _ a _) =
    a


after : Iterator a -> List a
after (Iterator _ _ xs) =
    xs
