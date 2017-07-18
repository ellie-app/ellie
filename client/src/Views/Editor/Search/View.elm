module Views.Editor.Search.View exposing (ViewModel, view)

import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Html exposing (Html, a, button, div, input, option, select, span, text)
import Html.Attributes as Attr exposing (href, id, placeholder, selected, target, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Shared.Icons as Icons
import Views.Editor.Search.Classes exposing (..)


type alias ViewModel msg =
    { onClose : msg
    , searchValue : String
    , onSearchChange : String -> msg
    , results : List Package
    , packages : List Package
    , onInstall : Package -> msg
    }


packageIsInstalled : List Package -> Package -> Bool
packageIsInstalled packageList ( n2, v2 ) =
    List.any
        (\( n1, _ ) -> n1 == n2)
        packageList


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
viewResultsItem onSelect (( name, version ) as package) =
    div [ class [ ResultsItem ] ]
        [ div [ class [ ResultsItemInfo ] ]
            [ div [ class [ ResultsItemName ] ]
                [ text <| name.user ++ "/" ++ name.project ]
            , div [ class [ ResultsItemVersion ] ]
                [ text <| "@" ++ Version.toString version ]
            ]
        , div [ class [ ResultsItemButtonGroup ] ]
            [ a
                [ class [ ResultsItemButton, ResultsItemButtonInner ]
                , href <| Package.docsLink package
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
                [ div [ class [ ResultsItemButtonInner ] ]
                    [ span [ class [ ResultsItemButtonIcon ] ]
                        [ Icons.plus ]
                    , span [ class [ ResultsItemButtonText ] ]
                        [ text "Install" ]
                    ]
                ]
            ]
        ]


viewResults : ViewModel msg -> Html msg
viewResults viewModel =
    if List.isEmpty viewModel.results then
        text ""
    else
        div [ class [ Results ] ]
            (viewModel.results
                |> List.filter (not << packageIsInstalled viewModel.packages)
                |> List.map (viewResultsItem viewModel.onInstall)
            )


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
