module Ellie.Ui.Sections exposing (..)

import Ellie.Ui.Icon as Icon
import Ellie.Ui.Sections.Styles as Styles
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Zipper as Zipper exposing (Zipper)


type alias Section msg =
    { title : String
    , icon : Icon.Icon
    , content : () -> Html msg
    , onSelect : msg
    }


view : Zipper (Section msg) -> Html msg
view sections =
    let
        before =
            sections
                |> Zipper.before
                |> List.map (viewSection False)

        current =
            sections
                |> Zipper.current
                |> viewSection True
                |> List.singleton

        after =
            sections
                |> Zipper.after
                |> List.map (viewSection False)
    in
    div [] <| before ++ current ++ after


viewSection : Bool -> Section msg -> Html msg
viewSection open sectionConfig =
    div []
        [ div []
            [ button
                [ Styles.button
                , Attributes.cond Styles.buttonOpen open
                , Attributes.cond
                    (onClick sectionConfig.onSelect)
                    (not open)
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
            ]
        , Html.viewIfLazy open <|
            \_ ->
                div [] [ sectionConfig.content () ]
        ]
