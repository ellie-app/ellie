module Components.Sidebar.View
    exposing
        ( view
        , ViewModel
        )

import Set exposing (Set)
import Html exposing (Html, a, label, textarea, h3, button, aside, div, text, span, input, hr, option, select)
import Html.Attributes exposing (href, target, type_, value, disabled)
import Html.Events exposing (onInput, onClick)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (fill, cx, cy, r, viewBox)
import Shared.Icons as Icons
import Components.Sidebar.Classes exposing (..)
import Types.Package as Package exposing (Package)
import Types.Version as Version exposing (Version)
import Shared.Utils as Utils exposing (renderIf)


type alias ViewModel msg =
    { title : String
    , description : String
    , onTitleChange : String -> msg
    , onDescriptionChange : String -> msg
    , packages : List Package
    , onAddPackageClick : msg
    , installingPackage : Maybe Package
    , removingHashes : Set String
    , onRemove : Package -> msg
    }


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


viewLoading : Package -> Html msg
viewLoading package =
    div [ class [ Loading ] ]
        [ div [ class [ LoadingPackageInfo ] ]
            [ text <| package.username ++ "/" ++ package.name ++ " @" ++ Version.toString package.version ]
        , div [ class [ LoadingAnimContainer ] ]
            [ svg [ viewBox "0 0 50 10" ]
                [ circle [ cx "5", cy "5", r "5" ] []
                , circle [ cx "25", cy "5", r "5" ] []
                , circle [ cx "45", cy "5", r "5" ] []
                ]
            ]
        ]


viewRemovingIndicator : Html msg
viewRemovingIndicator =
    button
        [ class [ PackagesItemButton ]
        , disabled True
        ]
        [ span [ class [ PackagesItemButtonIcon ] ]
            [ svg [ viewBox "0 0 50 10" ]
                [ circle [ cx "5", cy "5", r "5" ] []
                , circle [ cx "25", cy "5", r "5" ] []
                , circle [ cx "45", cy "5", r "5" ] []
                ]
            ]
        , span [ class [ PackagesItemButtonText ] ]
            [ text "Removing" ]
        ]


viewRemoveButton : msg -> Html msg
viewRemoveButton onRemove =
    button
        [ class [ PackagesItemButton ]
        , onClick onRemove
        ]
        [ div [ class [ PackagesItemButtonInner ] ]
            [ span [ class [ PackagesItemButtonIcon ] ]
                [ Icons.close ]
            , span [ class [ PackagesItemButtonText ] ]
                [ text "Remove" ]
            ]
        ]


viewDocsLink : Package -> Html msg
viewDocsLink package =
    a
        [ class [ PackagesItemButton ]
        , href <| Package.docsLink package
        , target "_blank"
        ]
        [ div [ class [ PackagesItemButtonInner ] ]
            [ span [ class [ PackagesItemButtonIcon ] ]
                [ Icons.link ]
            , span [ class [ PackagesItemButtonText ] ]
                [ text "Docs" ]
            ]
        ]


viewPackageItemVersion : Package -> Html msg
viewPackageItemVersion package =
    div [ class [ PackagesItemVersion ] ]
        [ text <| "@" ++ Version.toString package.version
        ]


viewPackageItemActions : Package -> ViewModel msg -> Html msg
viewPackageItemActions package viewModel =
    div [ class [ PackagesItemActions ] ]
        [ viewPackageItemVersion package
        , viewDocsLink package
        , viewRemoveButton (viewModel.onRemove package)
        ]


viewPackageItem : ViewModel msg -> Package -> Html msg
viewPackageItem viewModel package =
    let
        isRemoving =
            packageInHash viewModel.removingHashes package
    in
        div [ class [ PackagesItem ] ]
            [ div [ class [ PackagesItemName ] ]
                [ text <| package.username ++ " / " ++ package.name ]
            , renderIf isRemoving (\_ -> viewRemovingIndicator)
            , renderIf (not isRemoving) (\_ -> viewPackageItemActions package viewModel)
            ]


packageInHash : Set String -> Package -> Bool
packageInHash removingHashes package =
    Set.member (Package.hash package) removingHashes


viewPackages : ViewModel msg -> Html msg
viewPackages viewModel =
    div [ class [ Packages ] ]
        [ div [ class [ PackagesTitle ] ]
            [ text "Packages" ]
        , div [ class [ PackagesList ] ]
            (List.map (viewPackageItem viewModel) viewModel.packages)
        , viewModel.installingPackage
            |> Maybe.map viewLoading
            |> Maybe.withDefault (viewAddButton viewModel)
        ]


view : ViewModel msg -> Html msg
view viewModel =
    aside [ class [ Sidebar ] ]
        [ viewProjectInfo viewModel
        , viewPackages viewModel
        ]
