module Components.Sidebar.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Sidebar
    | Section
    | SectionHeader
    | SectionHeaderIcon
    | SectionHeaderText
    | Details
    | DetailsTitle
    | DetailsDescription
    | DetailsInputContainer
    | Dependencies
    | InstalledDeps
    | InstalledDepContainer
    | InstalledDepDetails
    | InstalledDepName
    | InstalledDepRange
    | InstalledDepMin
    | InstalledDepMax
    | InstalledDepRemoveContainer
    | AddDepButton
    | AddDepButtonText
    | AddDepButtonIcon
    | CloseButton


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_sidebar_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
