module Ellie.Ui.Button exposing (Action, Config, Size(..), Style(..), click, link, none, view)

import Ellie.Ui.Button.Styles as Styles
import Ellie.Ui.Icon as Icon
import Extra.Html.Attributes as Attributes
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (disabled, href, target)
import Html.Events exposing (onClick)


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
    , attributes : List (Html.Attribute msg)
    }


styleClass : Style -> Html.Attribute msg
styleClass style =
    case style of
        Primary ->
            Styles.primary

        Accent ->
            Styles.accent

        Link ->
            Styles.link


sizeClass : Size -> Html.Attribute msg
sizeClass size =
    case size of
        Small ->
            Styles.small

        Medium ->
            Styles.medium


iconSizeClass : Size -> Html.Attribute msg
iconSizeClass size =
    case size of
        Small ->
            Styles.iconSmall

        Medium ->
            Styles.iconMedium


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
                        a (attrs ++ config.attributes ++ [ href url, Attributes.cond (target "_blank") external ])

                None ->
                    a
    in
    elemFn
        [ Styles.button
        , disabled config.disabled
        , styleClass config.style
        , sizeClass config.size
        ]
        [ div [ Styles.inner ]
            [ case config.icon of
                Just icon ->
                    div
                        [ iconSizeClass config.size ]
                        [ Icon.view icon ]

                Nothing ->
                    text ""
            , div [] [ text config.label ]
            ]
        ]
