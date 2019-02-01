module Extra.Json.Decode exposing (either, genericUnion1, genericUnion2, genericUnion3, withDefault)

import Json.Decode as Decode exposing (Decoder)


withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    Decode.oneOf
        [ decoder
        , Decode.succeed default
        ]


either : Decoder x -> Decoder a -> Decoder (Result x a)
either x a =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Left" ->
                        x |> Decode.field "contents" |> Decode.map Err

                    "Right" ->
                        a |> Decode.field "contents" |> Decode.map Ok

                    _ ->
                        Decode.fail <| "Expecting an Either, got " ++ tag
            )


genericUnion1 : (a -> b) -> String -> Decoder a -> Decoder b
genericUnion1 constructor tag a =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\actualTag ->
                if actualTag == tag then
                    Decode.map constructor <| Decode.field "contents" a

                else
                    Decode.fail <| "Expected constructor \"" ++ tag ++ "\". Got \"" ++ actualTag ++ "\"."
            )


genericUnion2 : (a -> b -> c) -> String -> Decoder a -> Decoder b -> Decoder c
genericUnion2 constructor tag a b =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\actualTag ->
                if actualTag == tag then
                    Decode.field "contents" <|
                        Decode.map2 constructor
                            (Decode.index 0 a)
                            (Decode.index 1 b)

                else
                    Decode.fail <| "Expected constructor \"" ++ tag ++ "\". Got \"" ++ actualTag ++ "\"."
            )


genericUnion3 : (a -> b -> c -> d) -> String -> Decoder a -> Decoder b -> Decoder c -> Decoder d
genericUnion3 constructor tag a b c =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\actualTag ->
                if actualTag == tag then
                    Decode.field "contents" <|
                        Decode.map3 constructor
                            (Decode.index 0 a)
                            (Decode.index 1 b)
                            (Decode.index 2 c)

                else
                    Decode.fail <| "Expected constructor \"" ++ tag ++ "\". Got \"" ++ actualTag ++ "\"."
            )
