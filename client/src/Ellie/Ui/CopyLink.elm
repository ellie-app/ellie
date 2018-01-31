module Ellie.Ui.CopyLink exposing (Config, view)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Html.Styled exposing (Html, Attribute, button, div, input, label, text)
import Html.Styled.Attributes exposing (css, attribute, id, type_, value)


type alias Config =
    { id : String
    , url : String
    , title : String
    }


copy : String -> Attribute msg
copy id =
    attribute "onclick" <| "var el = document.getElementById(\"" ++ id ++ "\"); el.select(); document.execCommand('copy');"


view : Config -> Html msg
view config =
    div [ containerStyles ]
        [ label [ titleStyles ]
            [ text config.title ]
        , div [ controlsStyles ]
            [ button
                [ copy <| "copy_link_" ++ config.id
                , buttonStyles
                ]
                [ Icon.view Icon.Copy ]
            , input
                [ type_ "text"
                , value config.url
                , attribute "onclick" "this.blur();this.select();"
                , id <| "copy_link_" ++ config.id
                , inputStyles
                ]
                []
            ]
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ backgroundColor Colors.darkMediumGray
        , padding2 (px 4) (px 8)
        ]


buttonStyles : Attribute msg
buttonStyles =
    css
        [ width (px 28)
        , height (px 28)
        , marginRight (px 4)
        , property "background" "none"
        , border zero
        , cursor pointer
        , color Colors.mediumGray
        , hover [ color Colors.lightMediumGray ]
        , active [ color Colors.lightGray ]
        ]


inputStyles : Attribute msg
inputStyles =
    css
        [ border zero
        , property "background" "none"
        , fontFamily monospace
        , fontSize (px 15)
        , color Colors.lightGray
        , width (px 240)
        ]


controlsStyles : Attribute msg
controlsStyles =
    css [ displayFlex ]


titleStyles : Attribute msg
titleStyles =
    css
        [ display block
        , fontSize (px 12)
        , color Colors.lightMediumGray
        ]
