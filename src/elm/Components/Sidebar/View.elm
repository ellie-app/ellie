module Components.Sidebar.View
    exposing
        ( Context
        , view
        )

import Html exposing (Html, textarea, button, aside, div, text, span, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Shared.Icons as Icons
import Types.Dependency as Dependency exposing (Dependency)
import Types.Version as Version exposing (Version)
import Components.Sidebar.Classes exposing (..)
import Components.Sidebar.Update exposing (..)
import Components.Sidebar.Model as Model exposing (Model, SearchFlow(..))
import Components.PackageSearch.View as PackageSearch


closeButton : msg -> Html msg
closeButton msg =
    button
        [ class [ CloseButton ]
        , onClick msg
        ]
        [ Icons.close ]


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


installedDependency : msg -> Dependency -> Html msg
installedDependency onRemove dependency =
    div [ class [ InstalledDepContainer ] ]
        [ div [ class [ InstalledDepDetails ] ]
            [ div [ class [ InstalledDepName ] ]
                [ text <| dependency.username ++ "/" ++ dependency.name ]
            , div [ class [ InstalledDepRange ] ]
                [ span [ class [ InstalledDepMin ] ]
                    [ text <| Version.toString dependency.range.min ]
                , span [ class [ InstalledDepMax ] ]
                    [ text <| " <= v < " ++ Version.toString dependency.range.max ]
                ]
            ]
        , div [ class [ InstalledDepRemoveContainer ] ]
            [ closeButton onRemove
            ]
        ]


newPackageStuff : SearchFlow -> Html Msg
newPackageStuff searchFlow =
    case searchFlow of
        NotSearching ->
            button [ class [ AddDepButton ], onClick NewSearchStarted ]
                [ span [ class [ AddDepButtonIcon ] ] [ Icons.plusEmpty ]
                , span [ class [ AddDepButtonText ] ] [ text "Add Dependency" ]
                ]

        NewPackageSearching searchModel ->
            PackageSearch.view
                (PackageSearch.Context SearchCanceled (\_ -> NoOp) PackageSearchMsg)
                (searchModel)


dependencies : Context msg -> Model -> Html msg
dependencies context model =
    div [ class [ Dependencies ] ]
        [ div [ class [ InstalledDeps ] ]
            (List.map (installedDependency (context.onLocalMsg NoOp)) context.dependencies)
        , Html.map context.onLocalMsg <| newPackageStuff model.searchFlow
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
    , dependencies : List Dependency
    }



--
-- section
--     (context.onLocalMsg DetailsToggled)
--     (model.detailsOpen)
--     ("details")
--     (details context)
-- , section
--     (context.onLocalMsg PackagesToggled)
--     (model.packagesOpen)
--     ("packages")
--     (dependencies context model)


view : Context msg -> Html msg
view context =
    aside [ class [ Sidebar ] ]
        []
