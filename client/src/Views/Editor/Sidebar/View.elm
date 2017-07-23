module Views.Editor.Sidebar.View
    exposing
        ( ViewModel
        , view
        )

import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Html exposing (Attribute, Html, a, aside, button, div, h3, hr, input, label, node, option, select, span, text, textarea)
import Html.Attributes exposing (attribute, disabled, href, id, target, type_, value)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Native.CarbonAds
import Shared.Constants as Constants
import Shared.Icons as Icons
import Views.Editor.Sidebar.Classes exposing (..)


type alias ViewModel msg =
    { title : String
    , description : String
    , onTitleChange : String -> msg
    , onDescriptionChange : String -> msg
    , packages : List Package
    , onAddPackageClick : msg
    , onRemove : Package -> msg
    }


viewAd : Html msg
viewAd =
    Native.CarbonAds.ad
        Constants.carbonZoneId
        Constants.carbonServe
        Constants.carbonPlacement


viewProjectInfo : ViewModel msg -> Html msg
viewProjectInfo viewModel =
    div [ class [ ProjectInfo ] ]
        [ div [ class [ ProjectInfoTitle ] ]
            [ text "Project Info" ]
        , div []
            [ div [ class [ ProjectInfoInputContainer ] ]
                [ label
                    [ class [ ProjectInfoLabel ] ]
                    [ text "Title" ]
                , input
                    [ class [ ProjectInfoInput ]
                    , type_ "text"
                    , value viewModel.title
                    , onInput viewModel.onTitleChange
                    ]
                    []
                ]
            , div [ class [ ProjectInfoInputContainer ] ]
                [ label
                    [ class [ ProjectInfoLabel ] ]
                    [ text "Description" ]
                , textarea
                    [ class [ ProjectInfoTextarea ]
                    , value viewModel.description
                    , onInput viewModel.onDescriptionChange
                    ]
                    []
                ]
            ]
        ]


viewAddButton : ViewModel msg -> Html msg
viewAddButton viewModel =
    button
        [ class [ AddPackage ]
        , onClick viewModel.onAddPackageClick
        ]
        [ text "Add A Package" ]


viewRemoveButton : msg -> Html msg
viewRemoveButton onRemove =
    button
        [ class [ PackagesItemButton ]
        , onWithOptions
            "click"
            { preventDefault = True, stopPropagation = True }
            (Decode.succeed onRemove)
        ]
        [ div [ class [ PackagesItemButtonInner ] ]
            [ span [ class [ PackagesItemButtonIcon ] ]
                [ Icons.close ]
            , span [ class [ PackagesItemButtonText ] ]
                [ text "Remove" ]
            ]
        ]


viewPackageItemVersion : Package -> Html msg
viewPackageItemVersion ( name, version ) =
    div [ class [ PackagesItemVersion ] ]
        [ text <| "@" ++ Version.toString version
        ]


viewPackageItemActions : Package -> ViewModel msg -> Html msg
viewPackageItemActions package viewModel =
    div [ class [ PackagesItemActions ] ]
        [ viewPackageItemVersion package
        , viewRemoveButton (viewModel.onRemove package)
        ]


viewPackageItem : ViewModel msg -> Package -> Html msg
viewPackageItem viewModel (( name, version ) as package) =
    a
        [ class [ PackagesItem ]
        , href <| Package.docsLink package
        , target "_blank"
        ]
        [ div [ class [ PackagesItemName ] ]
            [ text <| name.user ++ " / " ++ name.project ]
        , viewPackageItemActions package viewModel
        ]


viewPackages : ViewModel msg -> Html msg
viewPackages viewModel =
    div [ class [ Packages ] ]
        [ div [ class [ PackagesTitle ] ]
            [ text "Packages" ]
        , div [ class [ PackagesList ] ]
            (List.map (viewPackageItem viewModel) viewModel.packages)
        , viewAddButton viewModel
        ]


viewTopStuff : ViewModel msg -> Html msg
viewTopStuff viewModel =
    div [ class [ TopStuff ] ]
        [ viewProjectInfo viewModel
        , viewPackages viewModel
        ]


viewBottomStuff : Html msg
viewBottomStuff =
    div [ class [ BottomStuff ] ]
        [ viewAd
        ]


view : ViewModel msg -> Html msg
view viewModel =
    aside [ class [ Sidebar ] ]
        [ viewTopStuff viewModel
        , viewBottomStuff
        ]
