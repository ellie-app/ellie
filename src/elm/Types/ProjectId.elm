module Types.ProjectId
    exposing
        ( ProjectId
        , EncodingVersion
        , latestVersion
        , encodingVersion
        , fromEncodedString
        , toEncodedString
        , fromIdString
        , fromIdStringWithVersion
        , toIdString
        )

import Types.BigNumber as BigNumber exposing (BigNumber, (|+|), (|*|), (|>|), (|/|), (|%|))


type EncodingVersion
    = V0
    | V1


latestVersion : EncodingVersion
latestVersion =
    V1


encodingVersion : ProjectId -> EncodingVersion
encodingVersion (ProjectId v _) =
    v


type ProjectId
    = ProjectId EncodingVersion String


alphabet : String
alphabet =
    "23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"


alphabetLength : Int
alphabetLength =
    String.length alphabet


charAt : Int -> String -> String
charAt index input =
    String.slice index (index + 1) input


indexOf : Char -> String -> Int
indexOf term space =
    String.indexes (String.fromChar term) space |> List.head |> Maybe.withDefault -1


fromEncodedStringV1 : String -> ProjectId
fromEncodedStringV1 input =
    let
        realInput =
            String.dropRight 2 input

        inputLength =
            String.length realInput

        bigNum =
            String.foldl
                (\nextChar out ->
                    out |*| alphabetLength |+| (indexOf nextChar alphabet)
                )
                BigNumber.zero
                realInput
    in
        bigNum
            |> BigNumber.toString
            |> ProjectId V1


fromEncodedStringV0 : String -> ProjectId
fromEncodedStringV0 input =
    let
        inputLength =
            String.length input

        bigNum =
            String.foldl
                (\nextChar out ->
                    (out |*| alphabetLength) |+| ((indexOf nextChar alphabet) + 1)
                )
                BigNumber.zero
                input
    in
        bigNum
            |> BigNumber.toString
            |> ProjectId V0


fromEncodedString : String -> ProjectId
fromEncodedString input =
    if String.endsWith "a1" input then
        fromEncodedStringV1 input
    else
        fromEncodedStringV0 input


fromIdStringWithVersion : EncodingVersion -> String -> ProjectId
fromIdStringWithVersion =
    ProjectId


fromIdString : String -> ProjectId
fromIdString =
    fromIdStringWithVersion latestVersion


toEncodedStringV1Help : String -> BigNumber -> String
toEncodedStringV1Help string bigNum =
    if bigNum |>| 0 then
        toEncodedStringV1Help
            ((charAt (BigNumber.toInt <| bigNum |%| alphabetLength) alphabet) ++ string)
            (BigNumber.floor <| bigNum |/| alphabetLength)
    else
        string


toEncodedStringV1 : String -> String
toEncodedStringV1 input =
    input
        |> BigNumber.fromString
        |> Result.map (toEncodedStringV1Help "")
        |> Result.map (\string -> string ++ "a1")
        |> Result.withDefault ""


toEncodedStringV0Help : String -> BigNumber -> String
toEncodedStringV0Help string bigNum =
    if bigNum |>| 0 then
        toEncodedStringV0Help
            ((charAt ((BigNumber.toInt <| bigNum |%| alphabetLength) - 1) alphabet) ++ string)
            (BigNumber.floor <| bigNum |/| alphabetLength)
    else
        string


toEncodedStringV0 : String -> String
toEncodedStringV0 input =
    input
        |> BigNumber.fromString
        |> Result.map (toEncodedStringV0Help "")
        |> Result.withDefault ""


toEncodedString : ProjectId -> String
toEncodedString (ProjectId version value) =
    case version of
        V1 ->
            toEncodedStringV1 value

        V0 ->
            toEncodedStringV0 value


toIdString : ProjectId -> String
toIdString (ProjectId _ value) =
    value
