module Ellie.Ui.Dropdown exposing (..)

import Ellie.Ui.Dropdown.Styles as Styles
import Ellie.Ui.Icon as Icon
import Html exposing (Html, div, option, select, text)
import Html.Attributes exposing (attribute, disabled, selected, value)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List


type alias Config a msg =
    { items : List a
    , label : a -> String
    , selected : a -> Bool
    , onChange : a -> msg
    , disabled : Bool
    }


view : Config a msg -> Html msg
view config =
    div [ Styles.container ]
        [ select
            [ on "change" (Decode.map config.onChange (selectedValue config.items))
            , value <| (config.items |> List.findIndex config.selected |> Maybe.withDefault -1 |> toString)
            , Styles.select
            , disabled config.disabled
            ]
          <|
            List.indexedMap
                (\i a ->
                    option
                        [ value <| toString i
                        , selected <| config.selected a
                        ]
                        [ text <| config.label a ]
                )
                config.items
        , div
            [ Styles.underline
            , attribute "data-underline" ""
            ]
            []
        , div
            [ Styles.arrow
            , attribute "data-arrow" ""
            ]
            [ Icon.view Icon.Chevron
            ]
        ]


selectedValue : List a -> Decoder a
selectedValue data =
    Decode.int
        |> Decode.at [ "target", "selectedIndex" ]
        |> Decode.andThen
            (\index ->
                data
                    |> List.drop index
                    |> List.head
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "index out of bounds")
            )
