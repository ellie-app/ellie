port module Pages.Editor.Logs.Subscriptions exposing (subscriptions)

import Data.Ellie.Log as Log exposing (Log)
import Json.Decode as Decode exposing (Decoder, Value)
import Pages.Editor.Logs.Model as Model exposing (Model)
import Pages.Editor.Logs.Update exposing (Msg(..))


port pagesEditorLogsIn : (Value -> msg) -> Sub msg


msgDecoder : Decoder Msg
msgDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "LogReceived" ->
                        Log.decoder
                            |> Decode.field "log"
                            |> Decode.map LogReceived

                    _ ->
                        Decode.succeed NoOp
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    pagesEditorLogsIn <|
        \value ->
            value
                |> Decode.decodeValue msgDecoder
                |> Result.withDefault NoOp
