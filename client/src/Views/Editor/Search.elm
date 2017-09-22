module Views.Editor.Search exposing (Config, view)

import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Html exposing (Html, a, button, div, input, option, select, span, text)
import Html.Attributes as Attr exposing (href, id, placeholder, selected, target, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Shared.Icons as Icons
import Views.Editor.Search.Styles as Styles
import Views.Modal as Modal


type alias Config msg =
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


viewSearchBar : Config msg -> Html msg
viewSearchBar config =
    div [ Styles.searchBar ]
        [ span [ Styles.searchBarIcon ]
            [ Icons.search ]
        , input
            [ type_ "text"
            , value config.searchValue
            , placeholder "Search for Elm packages"
            , onInput config.onSearchChange
            , Styles.searchBarInput
            , id "searchInput"
            ]
            []
        ]


viewResultsItem : (Package -> msg) -> Package -> Html msg
viewResultsItem onSelect (( name, version ) as package) =
    div [ Styles.resultsItem ]
        [ div [ Styles.resultsItemInfo ]
            [ div [ Styles.resultsItemName ]
                [ text <| name.user ++ "/" ++ name.project ]
            , div [ Styles.resultsItemVersion ]
                [ text <| "@" ++ Version.toString version ]
            ]
        , div [ Styles.resultsItemButtonGroup ]
            [ a
                [ Styles.resultsItemButton
                , Styles.resultsItemButtonInner
                , href <| Package.docsLink package
                , target "_blank"
                ]
                [ span [ Styles.resultsItemButtonIcon ]
                    [ Icons.link ]
                , span [ Styles.resultsItemButtonText ]
                    [ text "Docs" ]
                ]
            , button
                [ Styles.resultsItemButton
                , onClick <| onSelect package
                ]
                [ div [ Styles.resultsItemButtonInner ]
                    [ span [ Styles.resultsItemButtonIcon ]
                        [ Icons.plus ]
                    , span [ Styles.resultsItemButtonText ]
                        [ text "Install" ]
                    ]
                ]
            ]
        ]


viewResults : Config msg -> Html msg
viewResults config =
    if List.isEmpty config.results then
        text ""
    else
        div [ Styles.results ]
            (config.results
                |> List.filter (not << packageIsInstalled config.packages)
                |> List.map (viewResultsItem config.onInstall)
            )


view : Config msg -> Html msg
view config =
    Modal.view
        { onClose = Just config.onClose
        , content =
            [ viewSearchBar config
            , viewResults config
            ]
        }
