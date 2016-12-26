module Components.PackageSearch.View exposing (view, Context)

import Json.Decode as Decode
import Html exposing (Html, div, text, input, span, select, option)
import Html.Attributes as Attr exposing (type_, value, selected)
import Html.Events exposing (onInput, onClick, on)
import List.Nonempty as Nonempty exposing (Nonempty)
import Types.Dependency as Dependency exposing (Dependency)
import Types.Version as Version exposing (Version)
import Types.VersionRange as VersionRange exposing (VersionRange)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Shared.Icons as Icons
import Shared.Utils as Utils
import Components.PackageSearch.Model exposing (Model(..))
import Components.PackageSearch.Update exposing (Msg(..))
import Components.PackageSearch.Classes exposing (..)


packageAndVersionToDep : PackageSearchResult -> Version -> Dependency
packageAndVersionToDep package version =
    Dependency
        package.username
        package.name
        (VersionRange version (Version.nextMajor version))


onSelectedIndexChange : (Int -> msg) -> Html.Attribute msg
onSelectedIndexChange tagger =
    on "change" <| Decode.map tagger <| Decode.at [ "target", "selectedIndex" ] Decode.int


packageString : PackageSearchResult -> String
packageString package =
    package.username ++ "/" ++ package.name


packageItem : Context msg -> PackageSearchResult -> Html msg
packageItem context package =
    div
        [ onClick (PackageSelected package) |> Attr.map context.onLocalMsg ]
        [ text <| packageString package ]


sharedHash : { a | username : String, name : String } -> String
sharedHash a =
    a.username ++ "/" ++ a.name


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
        , div []
            (packages
                |> Utils.hashFilter sharedHash sharedHash context.excluded
                |> List.map (packageItem context)
            )
        , div []
            [ span
                [ class [ ActionIcon ]
                , onClick context.onCancel
                ]
                [ Icons.close ]
            ]
        ]


versionOption : Version -> Version -> Html msg
versionOption selectedVersion version =
    option
        [ selected <| Debug.log "s" (selectedVersion == version) ]
        [ text <| Version.toString version
        ]


versionSelect : Context msg -> PackageSearchResult -> Version -> Html msg
versionSelect context package version =
    div []
        [ div [] [ text <| packageString package ]
        , div []
            [ select
                [ onSelectedIndexChange VersionSelected |> Attr.map context.onLocalMsg ]
                (Nonempty.toList <| Nonempty.map (versionOption version) package.versions)
            , span [] [ text " <= v < " ]
            , span [] [ text <| Version.toString (Version.nextMajor version) ]
            ]
        , div []
            [ span
                [ class [ ActionIcon ]
                , onClick context.onCancel
                ]
                [ Icons.close ]
            , span
                [ class [ ActionIcon ]
                , onClick <| context.onApprove (packageAndVersionToDep package version)
                ]
                [ Icons.checkmark ]
            ]
        ]


type alias Context msg =
    { onCancel : msg
    , onApprove : Dependency -> msg
    , onLocalMsg : Msg -> msg
    , excluded : List Dependency
    }


view : Context msg -> Model -> Html msg
view context model =
    case model of
        Packages packages searchTerm ->
            packageSearch context packages searchTerm

        Versions package version ->
            versionSelect context package version
