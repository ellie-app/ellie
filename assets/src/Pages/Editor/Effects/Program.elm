module Pages.Editor.Effects.Program exposing (..)

import Css exposing (..)
import Css.Foreign
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode exposing (Decoder)
import Navigation
import Pages.Editor.Effects.Exception exposing (Exception)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Editor.Effects.State as State exposing (Msg(..), State)


type alias ProgramConfig route flags msg model =
    { subscriptions : model -> Inbound msg
    , update : msg -> model -> ( model, Outbound msg )
    , init : flags -> route -> ( model, Outbound msg )
    , view : model -> Html msg
    , error : Exception -> msg
    , flags : Decoder flags
    , url : Navigation.Location -> route
    , route : route -> msg
    , styles : List Css.Foreign.Snippet
    }


type alias EffectsProgram model msg =
    Program Decode.Value (State model msg) (Msg msg)


program : ProgramConfig route flags msg model -> EffectsProgram model msg
program config =
    let
        styles =
            Css.Foreign.global config.styles

        stateConfig =
            { onError = config.error
            , userUpdate = config.update
            , userSubs = config.subscriptions
            , userInit =
                \flags location ->
                    case Decode.decodeValue config.flags flags of
                        Ok decodedFlags ->
                            config.init decodedFlags (config.url location)

                        Err message ->
                            Debug.crash "bad flags"
            }
    in
    Navigation.programWithFlags (config.url >> config.route >> UserMsg)
        { view =
            \{ model } ->
                Html.toUnstyled <|
                    Html.div
                        [ css
                            [ height (pct 100)
                            ]
                        ]
                        [ styles
                        , Html.map UserMsg <| config.view model
                        , Html.node "ellie-ui-portal" [] []
                        ]
        , init = State.init stateConfig
        , subscriptions = State.subscriptions stateConfig
        , update = State.update stateConfig
        }
