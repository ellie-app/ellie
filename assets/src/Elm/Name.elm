module Elm.Name
    exposing
        ( Name
        , compare
        , fromString
        , toString
        )


type alias Name =
    { user : String
    , project : String
    }


toString : Name -> String
toString packageName =
    packageName.user ++ "/" ++ packageName.project


fromString : String -> Result String Name
fromString input =
    case String.split "/" input of
        [ user, project ] ->
            Ok <| Name user project

        _ ->
            Err "Expecting a name like USER/PROJECT"


compare : Name -> Name -> Order
compare left right =
    Basics.compare (toString left) (toString right)
