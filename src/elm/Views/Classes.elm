module Views.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = LoadingEditor
    | LoadingEditorGutter
    | LoadingEditorLineNumber
    | LoadingEditorLine
    | LoadingLines
    | LoadingShimmer
    | LoadingResults
    | LoadingResultsSection
    | LoadingResultsFullBox
    | LoadingResultsSplitContainer
    | LoadingResultsSplitLeft
    | LoadingResultsSplitRight
    | LoadingResultsCircle
    | Header
    | HeaderLogo
    | HeaderLogoText
    | HeaderStatus
    | HeaderStatusText
    | HeaderStatusEllipsis
    | HeaderButton
    | HeaderButtonIcon


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "views"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
