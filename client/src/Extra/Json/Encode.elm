module Extra.Json.Encode exposing (..)

import Json.Encode as Encode exposing (Value)


objectWithType : String -> List ( String, Value ) -> Value
objectWithType tipe data =
    Encode.object <|
        [ ( "type", Encode.string tipe ) ]
            ++ data


maybeNull : (a -> Value) -> Maybe a -> Value
maybeNull encoder value =
    value
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null


genericUnion : String -> List Value -> Value
genericUnion tag contents =
    case contents of
        [] ->
            Encode.object
                [ ( "tag", Encode.string tag ) ]

        entry :: [] ->
            Encode.object
                [ ( "tag", Encode.string tag )
                , ( "contents", entry )
                ]

        _ ->
            Encode.object
                [ ( "tag", Encode.string tag )
                , ( "contents", Encode.list contents )
                ]
