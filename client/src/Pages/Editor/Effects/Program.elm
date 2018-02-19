module Pages.Editor.Effects.Program exposing (..)

import Css.Foreign
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, Value)
import Navigation
import Pages.Editor.Effects.Error exposing (Error)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Effects.State as State exposing (Msg(..), State)


type alias ProgramConfig route flags msg model =
    { subscriptions : model -> Inbound msg
    , update : msg -> model -> ( model, Outbound msg )
    , init : flags -> route -> ( model, Outbound msg )
    , view : model -> Html msg
    , error : Error -> msg
    , flags : Decoder flags
    , url : Navigation.Location -> route
    , route : route -> msg
    , styles : List Css.Foreign.Snippet
    }


type alias EffectsProgram model msg =
    Program Value ( model, State msg ) (Msg msg)


program : ProgramConfig route flags msg model -> EffectsProgram model msg
program config =
    let
        styles =
            Css.Foreign.global config.styles
    in
    Navigation.programWithFlags (config.url >> config.route >> UserMsg)
        { view =
            \( model, _ ) ->
                Html.toUnstyled <|
                    Html.div []
                        [ styles
                        , Html.map UserMsg <| config.view model
                        , Html.node "ellie-ui-portal" [] []
                        ]
        , init =
            Outbound.wrapInit config.error <|
                \flags location ->
                    case Decode.decodeValue config.flags flags of
                        Ok decodedFlags ->
                            config.init decodedFlags (config.url location)

                        Err message ->
                            Debug.crash "bad flags"
        , subscriptions = Inbound.wrapSubs config.error config.subscriptions
        , update = Outbound.wrapUpdate config.error config.update
        }
