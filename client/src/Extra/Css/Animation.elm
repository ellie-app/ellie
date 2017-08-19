module Extra.Css.Animation exposing (KeyFrames, animation, compile, ease, from, infinite, iterate, keyFrames, linear, to)

import Css exposing (..)
import Dict
import Extra.String as String
import Hex
import Murmur3
import Time exposing (Time)


from : Pct
from =
    pct 0


to : Pct
to =
    pct 100


animation : Time -> TimingFunction -> Time -> IterationCount -> String -> String -> String -> KeyFrames -> Style
animation duration (TimingFunction timing) delay count direction fillMode playState (KeyFrames name _) =
    property "animation" <|
        String.fromFloat duration
            ++ "ms "
            ++ timing
            ++ " "
            ++ String.fromFloat delay
            ++ "ms "
            ++ iterationCountToString count
            ++ " "
            ++ direction
            ++ " "
            ++ fillMode
            ++ " "
            ++ playState
            ++ " "
            ++ name


keyFramesToPair : KeyFrames -> ( String, String )
keyFramesToPair (KeyFrames name css) =
    ( name, css )


type KeyFrames
    = KeyFrames String String


type TimingFunction
    = TimingFunction String


linear : TimingFunction
linear =
    TimingFunction "linear"


ease : TimingFunction
ease =
    TimingFunction "ease"


infinite : IterationCount
infinite =
    Infinite


iterate : Float -> IterationCount
iterate =
    NotInfinite


type IterationCount
    = Infinite
    | NotInfinite Float


iterationCountToString : IterationCount -> String
iterationCountToString count =
    case count of
        Infinite ->
            "infinite"

        NotInfinite value ->
            String.fromFloat value


stylesToStatement : List Style -> String
stylesToStatement styles =
    styles
        |> Css.asPairs
        |> List.map (\( key, value ) -> "    " ++ key ++ ": " ++ value ++ ";")
        |> List.sort
        |> String.join "\n"


compile : List (List KeyFrames) -> String
compile keyFrames =
    keyFrames
        |> List.concat
        |> List.map keyFramesToPair
        |> Dict.fromList
        |> Dict.values
        |> String.join "\n\n"


keyFrames : List ( Pct, List Style ) -> KeyFrames
keyFrames stops =
    let
        stopsCss =
            stops
                |> List.map (\( percent, styles ) -> ( percent.value, stylesToStatement styles ))
                |> List.sort
                |> List.map
                    (\( percent, styles ) ->
                        "  "
                            ++ percent
                            ++ " {\n"
                            ++ styles
                            ++ "\n  }"
                    )
                |> String.join "\n"

        hash =
            stopsCss
                |> Murmur3.hashString 0
                |> Hex.toString

        name =
            "animation" ++ hash

        fullCss =
            "@keyframes "
                ++ name
                ++ " {\n"
                ++ stopsCss
                ++ "\n}"
    in
    KeyFrames name fullCss
