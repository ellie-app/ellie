module Ellie.Ui.Button exposing (Action, Config, click, link, none, view)

import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (Attribute, Html, a, button, div, text)
import Html.Styled.Attributes as Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)


type Action msg
    = Click msg
    | Anchor Bool String
    | None


click : msg -> Action msg
click =
    Click


link : { href : String, external : Bool } -> Action msg
link { href, external } =
    Anchor external href


none : Action msg
none =
    None


type alias Config msg =
    { icon : Maybe Icon.Icon
    , label : String
    , disabled : Bool
    , action : Action msg
    }


view : Config msg -> Html msg
view config =
    let
        elemFn =
            case config.action of
                Click msg ->
                    \attrs ->
                        button (attrs ++ [ onClick msg ])

                Anchor external url ->
                    \attrs ->
                        a (attrs ++ [ href url, Attributes.cond (Attributes.target "_blank") external ])

                None ->
                    a
    in
    elemFn
        [ buttonStyles config
        , Attributes.disabled config.disabled
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                ]
            ]
            [ case config.icon of
                Just icon ->
                    div
                        [ css
                            [ width (px 19)
                            , height (px 18)
                            , padding2 (px 2) (px 4)
                            , paddingLeft zero
                            , borderRight3 (px 1) solid Theme.buttonBorder
                            , marginRight (px 4)
                            ]
                        , Attributes.attribute "data-ellie-ui-button-icon" ""
                        ]
                        [ Icon.view icon ]

                Nothing ->
                    text ""
            , div
                [ css [ padding2 (px 2) zero ] ]
                [ text config.label ]
            ]
        ]



-- Styles


buttonStyles : Config msg -> Attribute msg
buttonStyles config =
    css
        [ fontFamily inherit
        , fontSize (px 14)
        , textTransform uppercase
        , cursor pointer
        , textDecoration Css.none
        , property "user-select" "none"
        , backgroundColor Theme.buttonBackground
        , color Theme.primaryForeground
        , border3 (px 1) solid Theme.buttonBorder
        , padding2 (px 2) (px 4)
        , lineHeight (num 1)
        , transform Css.none
        , display inlineBlock
        , outline zero
        , focus
            [ borderColor Theme.accent
            , Css.Foreign.descendants
                [ Css.Foreign.selector "[data-ellie-ui-button-icon]"
                    [ borderColor Theme.accent
                    ]
                ]
            ]
        , active
            [ transform <| scale 1.1
            ]
        , disabled
            [ opacity (num 0.6)
            , cursor notAllowed
            ]
        , batch <|
            if config.disabled then
                [ opacity (num 0.6)
                , cursor notAllowed
                ]
            else
                []
        ]
