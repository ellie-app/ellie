module Ellie.Ui.Button exposing (Action, Config, Size(..), Style(..), click, link, none, view)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (Html, Attribute, a, button, div, text)
import Html.Styled.Attributes as Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)


type Size
    = Small
    | Medium


type Style
    = Primary
    | Accent
    | Link


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
    { size : Size
    , style : Style
    , icon : Maybe Icon.Icon
    , label : String
    , disabled : Bool
    , action : Action msg
    , attributes : List (Attribute msg)
    }


view : Config msg -> Html msg
view config =
    let
        elemFn =
            case config.action of
                Click msg ->
                    \attrs ->
                        button (attrs ++ config.attributes ++ [ onClick msg ])

                Anchor external url ->
                    \attrs ->
                        a (attrs ++ config.attributes ++ [ href url, Attributes.cond (Attributes.target "_blank") external ])

                None ->
                    a
    in
    elemFn
        [ buttonStyles config
        , Attributes.disabled config.disabled
        ]
        [ div [ innerStyles ]
            [ case config.icon of
                Just icon ->
                    div
                        [ iconStyles config.size ]
                        [ Icon.view icon ]

                Nothing ->
                    text ""
            , div [] [ text config.label ]
            ]
        ]


-- Styles

buttonStyles : Config msg -> Attribute msg
buttonStyles config =
    css
        [ border zero
        , fontFamily inherit
        , cursor pointer
        , textDecoration Css.none
        , property "user-select" "none"
        , disabled
            [ opacity (num 0.6)
            , cursor notAllowed
            ]
        , batch <|
            if config.disabled then
                [ hover [ color Colors.lightMediumGray |> important ]
                , cursor notAllowed
                ]
            else
                []
        , batch <|
            case config.style of
                Primary ->
                    [ backgroundColor Colors.mediumGray
                    , color Colors.lightGray
                    , backgroundShared
                    ]

                Accent ->
                    [ backgroundColor Colors.pink
                    , color Colors.lightGray
                    , backgroundShared
                    ]

                Link ->
                    [ color Colors.lightMediumGray
                    , padding zero |> important
                    , property "background" "none"
                    , fontWeight bold
                    , property "transition" "color 150ms"
                    , hover [ color Colors.lightGray ]
                    , disabled [ color Colors.lightMediumGray ]
                    ]
        , batch <|
            case config.size of
                Small ->
                    [ fontSize (px 12)
                    , padding2 (px 2) (px 4)
                    ]
                
                Medium ->
                    [ fontSize (px 15)
                    , padding2 (px 8) (px 12)
                    ]
        ]

    
backgroundShared : Css.Style
backgroundShared =
    batch
        [ borderRadius (px 2)
        , .bottom Colors.boxShadow
        , property "transition" "transform 150ms, box-shadow 150ms, background-color 150ms"
        , hover
            [ property "transform" "translateY(-1px)"
            , .bottomHover Colors.boxShadow
            ]
        , active
            [ property "transform" "none"
            ]
        , disabled
            [ property "transform" "none"
            , boxShadow Css.none
            , backgroundColor Colors.mediumGray
            ]
        ]


iconStyles : Size -> Attribute msg
iconStyles size =
    case size of
        Medium ->
            css
                [ height (px 16)
                , width (px 16)
                , marginRight (px 6)
                ]
        Small ->
            css
                [ height (px 12)
                , width (px 12)
                , marginRight (px 2)
                ]


innerStyles : Attribute msg
innerStyles =
    css
        [ displayFlex
        , alignItems center
        ]
