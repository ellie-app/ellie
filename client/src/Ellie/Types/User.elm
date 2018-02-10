module Ellie.Types.User
    exposing
        ( Id
        , User
        , decoder
        )

import Ellie.Types.Settings as Settings exposing (Settings)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set exposing (Set)


type alias Id =
    String


type alias User =
    { latestTermsVersion : Maybe Int
    , ownedProjects : Set String
    , settings : Settings
    }


decoder : Decoder User
decoder =
    Decode.decode User
        |> Decode.optional "acceptedTerms" (Decode.map Just Decode.int) Nothing
        |> Decode.required "ownedProjects" (Decode.map Set.fromList (Decode.list Decode.string))
        |> Decode.required "settings" Settings.decoder
