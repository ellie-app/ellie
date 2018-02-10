module Ellie.Types.Settings exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type Theme
    = Dark
    | Light


type alias Settings =
    { fontSize : String
    , fontFamily : String
    , theme : Theme
    , vimMode : Bool
    }


themeDecoder : Decoder Theme
themeDecoder =
    Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Dark" ->
                        Decode.succeed Dark

                    "Light" ->
                        Decode.succeed Light

                    _ ->
                        Decode.fail "Not a Theme constructor"
            )


decoder : Decoder Settings
decoder =
    Decode.decode Settings
        |> Decode.required "fontSize" Decode.string
        |> Decode.required "fontFamily" Decode.string
        |> Decode.required "theme" themeDecoder
        |> Decode.required "vimMode" Decode.bool
