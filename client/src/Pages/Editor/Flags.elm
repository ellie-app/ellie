module Pages.Editor.Flags exposing (Flags, decoder)

import Data.Jwt as Jwt exposing (Jwt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Window


type alias Flags =
    { windowSize : Window.Size
    , token : Maybe Jwt
    }


decoder : Decoder Flags
decoder =
    Decode.succeed Flags
        |> Decode.required "windowSize" windowSizeDecoder
        |> Decode.required "token" (Decode.nullable Jwt.decoder)


windowSizeDecoder : Decoder Window.Size
windowSizeDecoder =
    Decode.succeed Window.Size
        |> Decode.required "width" Decode.int
        |> Decode.required "height" Decode.int
