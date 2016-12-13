module Components.Sidebar.View
    exposing
        ( Context
        , view
        )

import Html exposing (Html, textarea, aside, div, text, span, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Shared.Icons as Icons
import Components.Sidebar.Classes exposing (..)
import Components.Sidebar.Update exposing (..)
import Components.Sidebar.Model as Model exposing (Model)


details : Context msg -> Html msg
details context =
    div
        [ class [ Details ] ]
        [ div [ class [ DetailsInputContainer ] ]
            [ input
                [ type_ "text"
                , value context.detailsTitle
                , onInput context.onTitleChange
                , class [ DetailsTitle ]
                ]
                []
            ]
        , div [ class [ DetailsInputContainer ] ]
            [ textarea
                [ value context.detailsDescription
                , onInput context.onDescriptionChange
                ]
                []
            ]
        ]


sectionHeader : msg -> Bool -> String -> Html msg
sectionHeader onToggle isOpen content =
    div
        [ class [ SectionHeader ]
        , onClick onToggle
        ]
        [ span [ class [ SectionHeaderText ] ]
            [ text content ]
        , span [ class [ SectionHeaderIcon ] ]
            [ if isOpen then
                Icons.minusEmpty
              else
                Icons.plusEmpty
            ]
        ]


section : msg -> Bool -> String -> Html msg -> Html msg
section onToggle isOpen header innerStuff =
    div [ class [ Section ] ]
        [ sectionHeader onToggle isOpen header
        , if isOpen then
            innerStuff
          else
            Html.text ""
        ]


type alias Context msg =
    { detailsTitle : String
    , detailsDescription : String
    , onLocalMsg : Msg -> msg
    , onTitleChange : String -> msg
    , onDescriptionChange : String -> msg
    }


view : Context msg -> Model -> Html msg
view context model =
    aside [ class [ Sidebar ] ]
        [ section
            (context.onLocalMsg DetailsToggled)
            (model.detailsOpen)
            ("details")
            (details context)
        , section
            (context.onLocalMsg PackagesToggled)
            (model.packagesOpen)
            ("packages")
            (Html.text "")
        ]
