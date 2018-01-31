module Ellie.Ui.Sections exposing (..)

import Colors
import Css exposing (..)
import Data.List.Iterator as Iterator exposing (Iterator)
import Ellie.Ui.Icon as Icon
import Extra.Html as Html
import Html.Styled exposing (Html, Attribute, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


type alias Section msg =
    { title : String
    , icon : Icon.Icon
    , content : () -> Html msg
    , onSelect : msg
    }


view : Iterator (Section msg) -> Html msg
view sections =
    let
        before =
            sections
                |> Iterator.before
                |> List.map (viewSection False)

        current =
            sections
                |> Iterator.current
                |> Maybe.map (viewSection True >> List.singleton)
                |> Maybe.withDefault []

        after =
            sections
                |> Iterator.after
                |> List.map (viewSection False)
    in
    div [ containerStyles ] <| before ++ current ++ after


viewSection : Bool -> Section msg -> Html msg
viewSection open sectionConfig =
    div [ sectionStyles open ]
        [ button
            [ buttonStyles open
            , onClick sectionConfig.onSelect
            ]
            [ div [ buttonInnerStyles ]
                [ div [ arrowStyles open ]
                    [ Icon.view Icon.Chevron ]
                , div [ iconStyles ] [ Icon.view sectionConfig.icon ]
                , div [] [ text sectionConfig.title ]
                ]
            ]
        , Html.viewIfLazy open <|
            \_ ->
                div [ contentStyles ] [ sectionConfig.content () ]
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , displayFlex
        , flexDirection column
        , height (pct 100)
        ]


sectionStyles : Bool -> Attribute msg
sectionStyles isOpen =
    css
        [ position relative
        , displayFlex
        , flexDirection column
        , width (pct 100)
        , if isOpen then
            batch
                [ flexShrink (int 1)
                , overflow hidden
                ]
          else
            batch [ flexShrink (int 0) ]
        ]


contentStyles : Attribute msg
contentStyles =
    css
        [ position relative
        , flexShrink (int 1)
        , overflowY auto
        ]


buttonStyles : Bool -> Attribute msg
buttonStyles isOpen =
    css
        [ display block
        , property "background" "none"
        , border zero
        , outline zero
        , backgroundColor Colors.darkGray
        , color Colors.lightGray
        , height (px 44)
        , padding2 zero (px 12)
        , width (pct 100)
        , fontFamily inherit
        , fontWeight bold
        , fontSize (px 16)
        , cursor pointer
        , flexShrink (int 0)
        , if isOpen then
            batch
                [ Colors.boxShadow |> .bottom
                , borderBottom3 (px 1) solid Colors.pink
                ]
          else
            batch []
        ]


buttonInnerStyles : Attribute msg
buttonInnerStyles =
    css
        [ displayFlex
        , alignItems center
        ]


arrowStyles : Bool -> Attribute msg
arrowStyles isOpen =
    css
        [ width (px 14)
        , height (px 14)
        , flexShrink (int 0)
        , marginRight (px 12)
        , transform <| rotate (deg 270)
        , if isOpen then
            batch
                [ color Colors.pink
                , transform none
                ]
          else
            batch []
        ]


iconStyles : Attribute msg
iconStyles =
    css
        [ width (px 16)
        , height (px 16)
        , flexShrink (int 0)
        , marginRight (px 12)
        ]
