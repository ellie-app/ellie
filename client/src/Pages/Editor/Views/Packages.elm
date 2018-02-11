module Pages.Editor.Views.Packages exposing (..)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.SplitPane as SplitPane
import Ellie.Ui.TextInput as TextInput
import Elm.Package as Package exposing (Package)
import Elm.Package.Name as Name
import Elm.Package.Searchable as Searchable exposing (Searchable)
import Elm.Package.Version as Version
import Extra.Maybe as Maybe
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, src)


type alias Config msg =
    { query : String
    , onSearch : String -> msg
    , installedPackages : List Package
    , searchedPackages : Maybe (List Searchable)
    , isLoading : Bool
    }


view : Config msg -> Html msg
view config =
    Html.div
        [ css
            [ height (pct 100)
            , displayFlex
            , flexDirection column
            ]
        ]
        [ viewHeader
        , viewSearchInput config.isLoading config.query config.onSearch
        , config.searchedPackages
            |> Maybe.map viewSearchedPackages
            |> Maybe.withDefaultLazy (\() -> viewInstalledPackages config.installedPackages)
        ]


viewHeader : Html msg
viewHeader =
    Html.div
        [ css
            [ fontSize (px 14)
            , fontWeight bold
            , color Colors.lightGray
            , padding (px 16)
            , textTransform uppercase
            , lineHeight (int 1)
            , flexShrink (int 0)
            ]
        ]
        [ Html.text "Packages" ]


viewSearchInput : Bool -> String -> (String -> msg) -> Html msg
viewSearchInput isLoading value onSearch =
    Html.div
        [ css
            [ padding4 zero (px 16) (px 16) (px 16)
            , flexShrink (int 0)
            ]
        ]
        [ TextInput.view
            { placeholder = "user/project"
            , value = value
            , clearable = True
            , onChange = onSearch
            , icon =
                if isLoading then
                    Just Icon.Loading
                else
                    Just Icon.Search
            }
        ]


viewSearchedPackages : List Searchable -> Html msg
viewSearchedPackages packages =
    List.map (.package >> viewPackage) packages
        |> Html.div
            [ css
                [ height (pct 100)
                , overflowY auto
                ]
            ]


viewInstalledPackages : List Package -> Html msg
viewInstalledPackages packages =
    Html.div
        []
        [ Html.div
            [ css
                [ color Colors.lightGray
                , textTransform uppercase
                , fontSize (px 15)
                , fontWeight bold
                , padding4 zero (px 16) (px 12) (px 16)
                , lineHeight (int 1)
                ]
            ]
            [ Html.text "Installed" ]
        , Html.div [] <| List.map viewPackage packages
        ]


viewPackage : Package -> Html msg
viewPackage ( name, version ) =
    Html.div
        [ css
            [ displayFlex
            , alignItems center
            , padding2 (px 12) (px 16)
            , backgroundColor Colors.darkMediumGray
            , marginBottom (px 2)
            ]
        ]
        [ Html.img
            [ src <| "https://github.com/" ++ name.user ++ ".png?size=48"
            , css
                [ width (px 48)
                , height (px 48)
                , flexShrink (int 0)
                ]
            ]
            []
        , Html.div [ infoContainerStyles ]
            [ Html.div [ packageNameStyles ]
                [ Html.text name.project ]
            , Html.div [ packageInfoStyles ] [ Html.text name.user ]
            , Html.div [ packageInfoStyles ] [ Html.text <| Version.toString version ]
            ]
        , Html.div
            [ css
                [ flexShrink (int 0)
                , alignSelf flexStart
                ]
            ]
            []
        ]


infoContainerStyles =
    css
        [ padding2 zero (px 16)
        , width (pct 100)
        , overflow hidden
        ]


packageNameStyles =
    css
        [ fontSize (px 24)
        , fontWeight bold
        , color Colors.lightGray
        , lineHeight (num 1.4)
        , textOverflow ellipsis
        , whiteSpace noWrap
        , overflow hidden
        ]


packageInfoStyles =
    css
        [ fontSize (px 18)
        , color Colors.lightMediumGray
        , lineHeight (num 1.2)
        , textOverflow ellipsis
        , whiteSpace noWrap
        , overflow hidden
        ]
