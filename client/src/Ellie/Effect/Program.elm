module Ellie.Effect.Program exposing (..)

import Css.Foreign
import Ellie.Effect.Error exposing (Error)
import Ellie.Effect.Inbound as Inbound exposing (Inbound)
import Ellie.Effect.Outbound as Outbound exposing (Outbound)
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, Value)
import Navigation


type alias ProgramConfig route flags msg model =
    { subscriptions : model -> List (Inbound msg)
    , update : msg -> model -> ( model, List (Outbound msg) )
    , start : msg
    , pass : msg
    , error : Error -> msg
    , flags : Decoder flags
    , url : Navigation.Location -> route
    , route : route -> msg
    , model : flags -> route -> model
    , view : model -> Html msg
    , styles : List Css.Foreign.Snippet
    }


program : ProgramConfig route flags msg model -> Program Value model msg
program config =
    let
        styles =
            Css.Foreign.global config.styles
    in
    Navigation.programWithFlags (config.url >> config.route)
        { view =
            \model ->
                Html.toUnstyled <| Html.div [] [ styles, config.view model ]
        , init =
            \flags location ->
                case Decode.decodeValue config.flags flags of
                    Ok decodedFlags ->
                        Outbound.wrap config.error <| config.update config.start (config.model decodedFlags (config.url location))

                    Err message ->
                        Debug.crash "bad flags"
        , subscriptions =
            \model ->
                Sub.batch <| List.map (Inbound.listen config.error config.pass) <| config.subscriptions model
        , update =
            \msg model ->
                Outbound.wrap config.error <| config.update msg model
        }
