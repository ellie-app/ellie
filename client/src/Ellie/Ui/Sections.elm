module Ellie.Ui.Sections exposing (..)

import Data.List.Iterator as Iterator exposing (Iterator)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Sections.Styles as Styles
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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
    div [ Styles.container ] <| before ++ current ++ after


viewSection : Bool -> Section msg -> Html msg
viewSection open sectionConfig =
    div
        [ Styles.section
        , Attributes.cond Styles.openSection open
        , Attributes.cond Styles.closedSection (not open)
        ]
        [ button
            [ Styles.button
            , Attributes.cond Styles.buttonOpen open
            , onClick sectionConfig.onSelect
            ]
            [ div [ Styles.buttonInner ]
                [ div
                    [ Styles.arrow
                    , Attributes.cond Styles.arrowOpen open
                    ]
                    [ Icon.view Icon.Chevron ]
                , div [ Styles.icon ] [ Icon.view sectionConfig.icon ]
                , div [] [ text sectionConfig.title ]
                ]
            ]
        , Html.viewIfLazy open <|
            \_ ->
                div [ Styles.content ] [ sectionConfig.content () ]
        ]
