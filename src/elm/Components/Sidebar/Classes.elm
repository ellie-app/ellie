module Components.Sidebar.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Sidebar
    | ProjectInfo
    | ProjectInfoInput
    | ProjectInfoLabel
    | ProjectInfoTitle
    | ProjectInfoInputContainer
    | ProjectInfoTextarea
    | Packages
    | PackagesList
    | PackagesTitle
    | PackagesItem
    | PackagesItemInfo
    | PackagesItemInfoName
    | PackagesItemInfoNameUsername
    | PackagesItemInfoVersion
    | PackagesItemRemove
    | PackagesItemRemoveIcon
    | PackagesItemRemoveText
    | AddPackage
    | Loading
    | LoadingPackageInfo
    | LoadingAnimContainer
    | LoadingAnim


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_sidebar_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
