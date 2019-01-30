module Ellie.Ui.Icon exposing (Icon(..), sprite, view)

import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
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
    | GithubProjects
    | Console
    | SmallLogo
    | Help
    | Slack
    | Search
    | Loading
    | Socket
    | Reload
    | More
    | Zip
    | Success
    | Failure
    | Warning
    | CloseAll
    | Notification
    | Debugger


view : Icon -> Html msg
view icon =
    svg [ iconStyles ]
        [ use [ xlinkHref <| "#icon-" ++ toIdString icon ] []
        ]


toIdString : Icon -> String
toIdString icon =
    case icon of
        Notification ->
            "notification"

        CloseAll ->
            "close-all"

        Failure ->
            "failure"

        Warning ->
            "warning"

        Success ->
            "success"

        Zip ->
            "zip"

        More ->
            "more"

        Reload ->
            "reload"

        Loading ->
            "loading"

        Search ->
            "search"

        Help ->
            "help"

        SmallLogo ->
            "small-logo"

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

        GithubProjects ->
            "github-projects"

        Console ->
            "console"

        Slack ->
            "slack"

        Socket ->
            "socket"

        Debugger ->
            "debugger"


iconStyles : Attribute msg
iconStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , fill currentColor
        , display block
        ]


sprite : Html msg
sprite =
    Html.node "ellie-ui-icon-sprite" [] []
