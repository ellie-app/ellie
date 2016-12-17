module Components.PackageSearch.View exposing (view, Context)

import Html exposing (Html, div, text, input, span)
import Html.Attributes as Attr exposing (type_, value)
import Html.Events exposing (onInput, onClick)
import Types.Dependency as Dependency exposing (Dependency)
import Types.Version as Version exposing (Version)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Shared.Icons as Icons
import Components.PackageSearch.Model exposing (Model(..))
import Components.PackageSearch.Update exposing (Msg(..))
import Components.PackageSearch.Classes exposing (..)


packageItem : Context msg -> PackageSearchResult -> Html msg
packageItem context package =
    div
        [ onClick (PackageSelected package) |> Attr.map context.onLocalMsg
        ]
        [ text <| PackageSearchResult.toString package ]


packageSearch : Context msg -> List PackageSearchResult -> String -> Html msg
packageSearch context packages searchTerm =
    div []
        [ div []
            [ input
                [ type_ "text"
                , value searchTerm
                , onInput PackageQueryUpdated |> Attr.map context.onLocalMsg
                ]
                []
            ]
        , div [] (List.map (packageItem context) packages)
        , div []
            [ span
                [ class [ ActionIcon ]
                , onClick context.onCancel
                ]
                [ Icons.close ]
            ]
        ]


versionItem : Context msg -> Version -> Html msg
versionItem context version =
    div
        [ onClick (VersionSelected version) |> Attr.map context.onLocalMsg
        ]
        [ text <| Version.toString version ]


versionSearch : Context msg -> PackageSearchResult -> List Version -> String -> Html msg
versionSearch context package versions searchTerm =
    div []
        [ div [] [ text <| PackageSearchResult.toString package ]
        , div []
            [ input
                [ type_ "text"
                , value searchTerm
                , onInput VersionQueryUpdated |> Attr.map context.onLocalMsg
                ]
                []
            ]
        , div [] (List.map (versionItem context) versions)
        , div []
            [ span
                [ class [ ActionIcon ]
                , onClick context.onCancel
                ]
                [ Icons.close ]
            ]
        ]


ready : Context msg -> Dependency -> Html msg
ready context dependency =
    div []
        [ div [] [ text <| (dependency.username ++ "/" ++ dependency.name) ]
        , div []
            [ span [] [ text <| Version.toString dependency.range.min ]
            , span [] [ text " <= v < " ]
            , span [] [ text <| Version.toString dependency.range.max ]
            ]
        , div []
            [ span
                [ class [ ActionIcon ]
                , onClick context.onCancel
                ]
                [ Icons.close ]
            , span [ class [ ActionIcon ] ] [ Icons.checkmark ]
            ]
        ]


type alias Context msg =
    { onCancel : msg
    , onApprove : Dependency -> msg
    , onLocalMsg : Msg -> msg
    }


view : Context msg -> Model -> Html msg
view context model =
    case model of
        Packages packages searchTerm ->
            packageSearch context packages searchTerm

        Versions package versions searchTerm ->
            versionSearch context package versions searchTerm

        Ready dependency ->
            ready context dependency
