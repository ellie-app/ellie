module Views.Output exposing (..)

import Colors
import Css exposing (..)
import Css.Elements exposing (..)
import Css.File exposing (..)


iframe : UniqueClass
iframe =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , border zero
        ]


overlay : UniqueClass
overlay =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , backgroundColor Colors.mediumGray_
        , color Colors.lightGray_
        , padding (px 16)
        ]


overlayContent : UniqueClass
overlayContent =
    uniqueClass
        [ top (pct 30)
        , position relative
        , width (pct 100)
        , displayFlex
        , property "align-items" "center"
        , flexDirection column
        ]


overlayTitle : UniqueClass
overlayTitle =
    uniqueClass
        [ property "font-family" "Leckerli One"
        , fontSize (px 48)
        , marginBottom (px 16)
        ]


overlaySubtitle : UniqueClass
overlaySubtitle =
    uniqueClass
        [ fontSize (px 24)
        , textAlign center
        , width (pct 100)
        , position relative
        , displayFlex
        , flexDirection column
        , property "align-items" "center"
        ]


errorsContainer : UniqueClass
errorsContainer =
    uniqueClass
        [ padding (px 16)
        , width (pct 100)
        , height (pct 100)
        , overflowY auto
        , maxWidth (px 500)
        ]


errorItem : UniqueClass
errorItem =
    uniqueClass
        [ padding (px 16)
        , backgroundColor Colors.darkGray_
        , marginBottom (px 2)
        , firstChild
            [ borderTopRightRadius (px 3)
            , borderTopLeftRadius (px 3)
            ]
        , lastChild
            [ borderBottomRightRadius (px 3)
            , borderBottomLeftRadius (px 3)
            ]
        , descendants
            [ code
                [ fontWeight (int 700)
                , backgroundColor (rgba 255 255 255 0.4)
                , padding2 (px 2) (px 6)
                , borderRadius (px 3)
                , display inlineBlock
                , lineHeight (int 1)
                ]
            ]
        ]


errorItemHeader : UniqueClass
errorItemHeader =
    uniqueClass
        [ displayFlex
        , property "justify-content" "space-between"
        , fontSize (px 10)
        , color Colors.lightMediumGray
        , lineHeight (px 10)
        , textTransform uppercase
        , paddingBottom (px 16)
        ]


errorItemName : UniqueClass
errorItemName =
    uniqueClass
        []


errorItemLocation : UniqueClass
errorItemLocation =
    uniqueClass
        []


errorItemOverview : UniqueClass
errorItemOverview =
    uniqueClass
        [ fontSize (px 16)
        , fontWeight (int 700)
        , paddingBottom (px 16)
        , lineHeight (px 16)
        ]


errorItemDetails : UniqueClass
errorItemDetails =
    uniqueClass
        [ fontSize (px 14)
        , lineHeight (px 14)
        ]


manyModulesWarning : UniqueClass
manyModulesWarning =
    uniqueClass
        [ fontSize (px 18)
        , maxWidth (px 450)
        ]
