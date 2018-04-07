module Ellie.Types.Settings exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


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
                    "DARK" ->
                        Decode.succeed Dark

                    "LIGHT" ->
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


themeEncoder : Theme -> Value
themeEncoder theme =
    case theme of
        Dark ->
            Encode.string "DARK"

        Light ->
            Encode.string "LIGHT"


encoder : Settings -> Value
encoder settings =
    Encode.object
        [ ( "fontSize", Encode.string settings.fontSize )
        , ( "fontFamily", Encode.string settings.fontFamily )
        , ( "vimMode", Encode.bool settings.vimMode )
        , ( "theme", themeEncoder settings.theme )
        ]
