module Ellie.Ui.Icon exposing (Icon(..), view)

import Ellie.Ui.Icon.Styles as Styles
import Html exposing (Html)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


type Icon
    = Settings
    | Package
    | Trash
    | Fork
    | Tag
    | Install
    | Info
    | Play
    | Upload
    | Format
    | Link
    | People
    | Close
    | Fold
    | Unfold
    | ElmLogo
    | HtmlTag
    | Eye
    | External
    | Chevron
    | Document
    | Copy


view : Icon -> Html msg
view icon =
    svg [ Styles.icon ]
        [ use [ xlinkHref <| "#icon-" ++ toIdString icon ] []
        ]


toIdString : Icon -> String
toIdString icon =
    case icon of
        Settings ->
            "settings"

        Package ->
            "package"

        Trash ->
            "trash"

        Fork ->
            "fork"

        Tag ->
            "tag"

        Install ->
            "install"

        Info ->
            "info"

        Play ->
            "play"

        Upload ->
            "upload"

        Format ->
            "format"

        Link ->
            "link"

        People ->
            "people"

        Close ->
            "close"

        Fold ->
            "fold"

        Unfold ->
            "unfold"

        ElmLogo ->
            "elm-loco"

        HtmlTag ->
            "html-tag"

        Eye ->
            "eye"

        External ->
            "external"

        Chevron ->
            "chevron"

        Document ->
            "document"

        Copy ->
            "copy"
