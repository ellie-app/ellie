module Data.Uuid exposing (Uuid, eq, fromString, toString)


type Uuid
    = Uuid String


fromString : String -> Uuid
fromString =
    Uuid


toString : Uuid -> String
toString (Uuid string) =
    string


eq : Uuid -> Uuid -> Bool
eq (Uuid left) (Uuid right) =
    left == right
