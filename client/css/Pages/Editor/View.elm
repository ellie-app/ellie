module Pages.Editor.View exposing (..)

import Colors as Colors
import Constants as Constants
import Css exposing (..)
import Css.Elements exposing (..)
import Css.Extra exposing (..)
import Css.File exposing (..)


htmlStyles : Snippet
htmlStyles =
    html
        [ height (pct 100)
        , backgroundColor Colors.darkGray
        ]


bodyStyles : Snippet
bodyStyles =
    body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ Constants.sansFont ]
        , property "-webkit-font-smoothing" "antialiased"
        ]


everythingStyles : Snippet
everythingStyles =
    everything
        [ boxSizing borderBox ]


buttonStyles : Snippet
buttonStyles =
    button
        [ focus [ outline zero ] ]


inputStyles : Snippet
inputStyles =
    input [ focus [ outline zero ] ]


elmEditor : Snippet
elmEditor =
    id "elmEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 0)
        ]


htmlEditor : Snippet
htmlEditor =
    id "htmlEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        ]


appContainer : UniqueClass
appContainer =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , displayFlex
        , position relative
        ]


workArea : UniqueClass
workArea =
    uniqueClass
        [ width <| calc (pct 100) minus (px 240)
        , height (pct 100)
        , displayFlex
        , position relative
        ]


mainContainer : UniqueClass
mainContainer =
    uniqueClass
        [ width (pct 100)
        , height <| calc (pct 100) minus (px 40)
        , displayFlex
        , position relative
        , zIndex (int 1)
        ]


appContainerInner : UniqueClass
appContainerInner =
    uniqueClass
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , property "transition" "filter 0.3s 0.2s"
        ]


loadingRevision : UniqueClass
loadingRevision =
    uniqueClass
        [ filter <| blur (px 30)
        ]


editorsContainer : UniqueClass
editorsContainer =
    uniqueClass
        [ displayFlex
        , position relative
        , zIndex (int 0)
        , flexDirection column
        , height (pct 100)
        , width (pct 50)
        , borderRight3 (px 2) solid Colors.mediumGray
        , overflow hidden
        ]


editorContainer : UniqueClass
editorContainer =
    uniqueClass
        [ height (pct 50)
        , position relative
        , backgroundColor Colors.darkGray
        , firstChild
            [ borderBottom3 (px 1) solid Colors.mediumGray
            ]
        , lastChild
            [ borderTop3 (px 1) solid Colors.mediumGray
            ]
        ]


editorContainerCollapse : UniqueClass
editorContainerCollapse =
    uniqueClass
        [ height (px 40)
        ]


editorContainerFull : UniqueClass
editorContainerFull =
    uniqueClass
        [ height <| calc (pct 100) minus (px 40)
        ]


outputContainer : UniqueClass
outputContainer =
    uniqueClass
        [ width (pct 50)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        , boxShadow5 (px -2) zero (px 8) zero (rgba 0 0 0 0.5)
        , overflow hidden
        ]


notificationsContainer : UniqueClass
notificationsContainer =
    uniqueClass
        [ position absolute
        , right (px 16)
        , top (px 16)
        ]


outputResizeHandle : UniqueClass
outputResizeHandle =
    uniqueClass
        [ position absolute
        , width (px 6)
        , height (pct 100)
        , marginLeft (px -3)
        , cursor ewResize
        , zIndex (int 6)
        ]


editorResizeHandle : UniqueClass
editorResizeHandle =
    uniqueClass
        [ position absolute
        , height (px 6)
        , width (pct 100)
        , marginTop (px -3)
        , cursor nsResize
        , zIndex (int 6)
        ]


resizeNs : UniqueClass
resizeNs =
    uniqueClass
        [ descendants [ everything [ cursor nsResize |> important ] ]
        , cursor nsResize
        ]


resizeEw : UniqueClass
resizeEw =
    uniqueClass
        [ descendants [ everything [ cursor ewResize |> important ] ]
        , cursor ewResize
        ]


embedLinkContainer : UniqueClass
embedLinkContainer =
    uniqueClass
        [ position absolute
        , zIndex (int 2)
        , width (px 320)
        , left (px 448)
        , top (px 16)
        ]


overlayButton : UniqueClass
overlayButton =
    uniqueClass
        [ position absolute
        , property "background" "none"
        , border zero
        , color Colors.lightGray
        , displayFlex
        , zIndex (int 7)
        , cursor pointer
        , borderRadius (px 3)
        , padding (px 6)
        , Colors.darkGray
            |> withAlpha 0.5
            |> backgroundColor
        ]


collapseButton : UniqueClass
collapseButton =
    uniqueClass
        [ top (px 6)
        , right (px 6)
        ]


overlayButtonText : UniqueClass
overlayButtonText =
    uniqueClass
        [ fontSize (px 16)
        , textTransform uppercase
        , lineHeight (px 16)
        ]


overlayButtonIcon : UniqueClass
overlayButtonIcon =
    uniqueClass
        [ marginLeft (px 6)
        , display block
        , width (px 16)
        , height (px 16)
        ]
