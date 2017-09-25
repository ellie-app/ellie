module Ellie.Ui.CopyLink exposing (..)

import Ellie.Ui.CopyLink.Styles as Styles
import Ellie.Ui.Icon as Icon
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (attribute, id, type_, value)


type alias Config =
    { id : String
    , url : String
    , title : String
    }


copy : String -> Html.Attribute msg
copy id =
    attribute "onclick" <| "var el = document.getElementById(\"" ++ id ++ "\"); el.select(); document.execCommand('copy');"


view : Config -> Html msg
view config =
    div [ Styles.container ]
        [ label [ Styles.title ] [ text config.title ]
        , div [ Styles.controls ]
            [ button
                [ copy <| "copy_link_" ++ config.id
                , Styles.button
                ]
                [ Icon.view Icon.Copy ]
            , input
                [ type_ "text"
                , value config.url
                , attribute "onclick" "this.blur();this.select();"
                , id <| "copy_link_" ++ config.id
                , Styles.input
                ]
                []
            ]
        ]
