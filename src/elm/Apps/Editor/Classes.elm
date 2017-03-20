module Apps.Editor.Classes exposing (Classes(..), class, classList)

import Html exposing (Attribute)
import Html.CssHelpers


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "app_"


class : List class -> Html.Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Html.Attribute msg
classList =
    helpers.classList


type Classes
    = AppContainer
    | MainContainer
    | WorkArea
    | EditorsContainer
    | EditorContainerCollapse
    | EditorContainerFull
    | EditorContainer
    | LoadedContainer
    | OutputContainer
    | NotificationsContainer
    | OutputResizeHandle
    | ResizeNs
    | ResizeEw
    | EditorResizeHandle
    | EmbedLinkContainer
    | LoadingRevision
    | AppContainerInner
    | CollapseButton
    | CollapseButtonText
    | CollapseButtonIcon
