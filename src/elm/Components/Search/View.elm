module Components.Search.View exposing (view, ViewModel)

import Html exposing (Html, div, text, input, button, span, select, option, a)
import Html.Attributes as Attr exposing (type_, value, selected, placeholder, href, id, target)
import Html.Events exposing (onInput, onClick, on)
import Components.Search.Classes exposing (..)
import Shared.Icons as Icons
import Types.Package as Package exposing (Package)
import Types.Version as Version exposing (Version)


type alias ViewModel msg =
    { onClose : msg
    , searchValue : String
    , onSearchChange : String -> msg
    , results : List Package
    , onInstall : Package -> msg
    }


docsLink : Package -> String
docsLink package =
    "http://package.elm-lang.org/packages/"
        ++ package.username
        ++ "/"
        ++ package.name
        ++ "/"
        ++ Version.toString package.version


viewSearchBar : ViewModel msg -> Html msg
viewSearchBar viewModel =
    div [ class [ SearchBar ] ]
        [ span [ class [ SearchBarIcon ] ]
            [ Icons.search ]
        , input
            [ type_ "text"
            , value viewModel.searchValue
            , placeholder "Search for Elm packages"
            , onInput viewModel.onSearchChange
            , class [ SearchBarInput ]
            , id "searchInput"
            ]
            []
        ]


viewResultsItem : (Package -> msg) -> Package -> Html msg
viewResultsItem onSelect package =
    div [ class [ ResultsItem ] ]
        [ div [ class [ ResultsItemInfo ] ]
            [ div [ class [ ResultsItemName ] ]
                [ text <| package.username ++ "/" ++ package.name ]
            , div [ class [ ResultsItemVersion ] ]
                [ text <| "@" ++ Version.toString package.version ]
            ]
        , div [ class [ ResultsItemButtonGroup ] ]
            [ a
                [ class [ ResultsItemButton ]
                , href <| docsLink package
                , target "_blank"
                ]
                [ span [ class [ ResultsItemButtonIcon ] ]
                    [ Icons.link ]
                , span [ class [ ResultsItemButtonText ] ]
                    [ text "Docs" ]
                ]
            , button
                [ class [ ResultsItemButton ]
                , onClick <| onSelect package
                ]
                [ span [ class [ ResultsItemButtonIcon ] ]
                    [ Icons.plus ]
                , span [ class [ ResultsItemButtonText ] ]
                    [ text "Install" ]
                ]
            ]
        ]


viewResults : ViewModel msg -> Html msg
viewResults viewModel =
    if List.isEmpty viewModel.results then
        text ""
    else
        div [ class [ Results ] ]
            (List.map (viewResultsItem viewModel.onInstall) viewModel.results)


view : ViewModel msg -> Html msg
view viewModel =
    div [ class [ Container ] ]
        [ div
            [ class [ Backdrop ]
            , onClick viewModel.onClose
            ]
            []
        , viewSearchBar viewModel
        , viewResults viewModel
        ]
