module Pages.Editor.Flags exposing (Flags, decoder)

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Window


type alias Flags =
    { windowSize : Window.Size
    , online : Bool
    , vimMode : Bool
    , latestTermsVersion : TermsVersion
    , acceptedTermsVersion : Maybe TermsVersion
    }


decoder : Decoder Flags
decoder =
    Decode.succeed Flags
        |> Decode.required "windowSize" windowSizeDecoder
        |> Decode.required "online" Decode.bool
        |> Decode.required "vimMode" Decode.bool
        |> Decode.required "latestTermsVersion" TermsVersion.decoder
        |> Decode.required "acceptedTermsVersion" (Decode.nullable TermsVersion.decoder)


windowSizeDecoder : Decoder Window.Size
windowSizeDecoder =
    Decode.succeed Window.Size
        |> Decode.required "width" Decode.int
        |> Decode.required "height" Decode.int
