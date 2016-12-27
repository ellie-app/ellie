module Components.Sidebar.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Sidebar
    | Section
    | SectionHeader
    | SectionContent
    | TextInput
    | Textarea
    | PackagesList
    | DepItem
    | DepItemDetails
    | IconButton
    | DepItemPackageName
    | AddDepButton
    | AddDepButtonIcon
    | AddDepContainer
    | AddDepInputContainer
    | AddDepResultList
    | AddDepPackageSearch
    | AddDepPackageItem
    | AddDepVersionPackageName
    | AddDepVersion
    | AddDepVersionDetails


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_sidebar_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
