module Views.Editor.Sidebar.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Sidebar
    | TopStuff
    | BottomStuff
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
    | PackagesItemVersion
    | PackagesItemActions
    | PackagesItemName
    | PackagesItemButton
    | PackagesItemButtonInner
    | PackagesItemButtonIcon
    | PackagesItemButtonText
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
