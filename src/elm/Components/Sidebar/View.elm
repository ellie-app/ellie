module Components.Sidebar.View
    exposing
        ( view
        , ViewModel
        )

import Set exposing (Set)
import Html exposing (Html, label, textarea, h3, button, aside, div, text, span, input, hr, option, select)
import Html.Attributes exposing (type_, value, disabled)
import Html.Events exposing (onInput, onClick)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (fill, cx, cy, r, viewBox)
import Shared.Icons as Icons
import Components.Sidebar.Classes exposing (..)
import Types.Dependency as Dependency exposing (Dependency)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.Package as Package exposing (Package)
import Types.Version as Version exposing (Version)
import Shared.Utils as Utils exposing (renderIf)


type alias ViewModel msg =
    { title : String
    , description : String
    , onTitleChange : String -> msg
    , onDescriptionChange : String -> msg
    , dependencies : List Dependency
    , onAddPackageClick : msg
    , installingPackage : Maybe Package
    , removingHashes : Set String
    , onRemove : Dependency -> msg
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
        [ class [ PackagesItemRemove ]
        , disabled True
        ]
        [ span [ class [ PackagesItemRemoveIcon ] ]
            [ svg [ viewBox "0 0 50 10" ]
                [ circle [ cx "5", cy "5", r "5" ] []
                , circle [ cx "25", cy "5", r "5" ] []
                , circle [ cx "45", cy "5", r "5" ] []
                ]
            ]
        , span [ class [ PackagesItemRemoveText ] ]
            [ text "Removing" ]
        ]


viewRemoveButton : msg -> Html msg
viewRemoveButton onRemove =
    button
        [ class [ PackagesItemRemove ]
        , onClick onRemove
        ]
        [ div [ class [ PackagesItemRemoveInner ] ]
            [ span [ class [ PackagesItemRemoveIcon ] ]
                [ Icons.close ]
            , span [ class [ PackagesItemRemoveText ] ]
                [ text "Remove" ]
            ]
        ]


viewPackageItem : ViewModel msg -> Dependency -> Html msg
viewPackageItem viewModel dependency =
    let
        isRemoving =
            depInHash viewModel.removingHashes dependency
    in
        div [ class [ PackagesItem ] ]
            [ div [ class [ PackagesItemInfo ] ]
                [ div [ class [ PackagesItemInfoName ] ]
                    [ span [ class [ PackagesItemInfoNameUsername ] ]
                        [ text <| dependency.username ++ " / " ]
                    , span [] [ text dependency.name ]
                    ]
                , div [ class [ PackagesItemInfoVersion ] ]
                    [ text <| VersionRange.toString dependency.range ]
                ]
            , renderIf isRemoving (\_ -> viewRemovingIndicator)
            , renderIf (not isRemoving) (\_ -> viewRemoveButton (viewModel.onRemove dependency))
            ]


depInHash : Set String -> Dependency -> Bool
depInHash removingHashes dependency =
    Set.member (Dependency.hash dependency) removingHashes


viewPackages : ViewModel msg -> Html msg
viewPackages viewModel =
    div [ class [ Packages ] ]
        [ div [ class [ PackagesTitle ] ]
            [ text "Packages" ]
        , div [ class [ PackagesList ] ]
            (List.map (viewPackageItem viewModel) viewModel.dependencies)
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
