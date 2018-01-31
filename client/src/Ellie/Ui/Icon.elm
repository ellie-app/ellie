module Ellie.Ui.Icon exposing (Icon(..), view)

import Css exposing (..)
import Html.Styled exposing (Html, Attribute)
import Svg.Styled exposing (svg, use)
import Svg.Styled.Attributes exposing (css, xlinkHref)


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
    | GitHub
    | Trello
    | Console


view : Icon -> Html msg
view icon =
    svg [ iconStyles ]
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
            "elm-logo"

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

        GitHub ->
            "github"

        Trello ->
            "trello"

        Console ->
            "console"


iconStyles : Attribute msg
iconStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , fill currentColor
        , display block
        ]