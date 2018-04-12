module Pages.Editor.Views.Packages exposing (..)

import Colors
import Css exposing (..)
import Ellie.Constants as Constants
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.TextInput as TextInput
import Elm.Package as Package exposing (Package)
import Elm.Version as Version
import Extra.Maybe as Maybe
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, src)


type alias Config msg =
    { query : String
    , onSearch : String -> msg
    , installedPackages : List Package
    , searchedPackages : Maybe (List Package)
    , isLoading : Bool
    , onUninstall : Package -> msg
    , onInstall : Package -> msg
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
            |> Maybe.map (viewSearchedPackages config.onInstall config.onUninstall config.installedPackages)
            |> Maybe.withDefaultLazy
                (\() ->
                    viewInstalledPackages
                        config.onUninstall
                        config.installedPackages
                )
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
            , autofocus = True
            , icon =
                if isLoading then
                    Just Icon.Loading
                else
                    Just Icon.Search
            }
        ]


viewSearchedPackages : (Package -> msg) -> (Package -> msg) -> List Package -> List Package -> Html msg
viewSearchedPackages onInstall onUninstall installed packages =
    packages
        |> List.map (viewSearchedPackage onInstall onUninstall installed)
        |> Html.div
            [ css
                [ height (pct 100)
                , overflowY auto
                ]
            ]


viewInstalledPackages : (Package -> msg) -> List Package -> Html msg
viewInstalledPackages onUninstall packages =
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
        , Html.div [] <| List.map (viewInstalledPackage onUninstall) packages
        ]


viewInstalledPackage : (Package -> msg) -> Package -> Html msg
viewInstalledPackage onUninstall ({ name, version } as package) =
    viewPackage package
        [ Button.view
            { icon = Just Icon.Trash
            , label = "Uninstall"
            , disabled = False
            , action = Button.click <| onUninstall package
            }
        , Button.view
            { icon = Just Icon.Document
            , label = "View Docs"
            , disabled = False
            , action =
                Button.link
                    { external = True
                    , href =
                        Constants.packageSite
                            ++ "/packages/"
                            ++ name.user
                            ++ "/"
                            ++ name.project
                            ++ "/"
                            ++ Version.toString version
                    }
            }
        ]


viewSearchedPackage : (Package -> msg) -> (Package -> msg) -> List Package -> Package -> Html msg
viewSearchedPackage onInstall onUninstall installed package =
    if List.member package installed then
        viewInstalledPackage onUninstall package
    else
        viewPackage package
            [ Button.view
                { icon = Just Icon.Install
                , label = "Install"
                , disabled = False
                , action = Button.click <| onInstall package
                }
            ]


viewPackage : Package -> List (Html msg) -> Html msg
viewPackage { name, version } actions =
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
                , displayFlex
                , flexDirection column
                , alignItems center
                ]
            ]
            (List.map viewAction actions)
        ]


viewAction : Html msg -> Html msg
viewAction action =
    Html.div
        [ css [ paddingBottom (px 8) ]
        ]
        [ action
        ]


infoContainerStyles : Html.Attribute msg
infoContainerStyles =
    css
        [ padding2 zero (px 16)
        , width (pct 100)
        , overflow hidden
        ]


packageNameStyles : Html.Attribute msg
packageNameStyles =
    css
        [ fontSize (px 20)
        , fontWeight bold
        , color Colors.lightGray
        , lineHeight (num 1.4)
        , textOverflow ellipsis
        , whiteSpace noWrap
        , overflow hidden
        ]


packageInfoStyles : Html.Attribute msg
packageInfoStyles =
    css
        [ fontSize (px 18)
        , color Colors.lightMediumGray
        , lineHeight (num 1.2)
        , textOverflow ellipsis
        , whiteSpace noWrap
        , overflow hidden
        ]
