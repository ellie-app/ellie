module Data.Url
    exposing
        ( Url
        , fromString
        , toString
        )


type Url
    = Url String


fromString : String -> Url
fromString =
    Url


toString : Url -> String
toString (Url string) =
    string
