module Views.Editor.Sidebar exposing (..)

import Colors as Colors
import Constants as Constants
import Css exposing (..)
import Css.File exposing (..)


sidebar : UniqueClass
sidebar =
    uniqueClass
        [ height (pct 100)
        , width (px Constants.sidebarWidth)
        , backgroundColor (hex Colors.mediumGray)
        , padding2 (px 8) (px 16)
        , property "box-shadow" "2px 0 8px 0px rgba(0,0,0,0.50)"
        , position relative
        , property "z-index" "1"
        , displayFlex
        , flexDirection column
        , property "justify-content" "space-between"
        , overflow hidden
        ]


topStuff : UniqueClass
topStuff =
    uniqueClass
        [ displayFlex
        , flexDirection column
        , property "flex-shrink" "1"
        , overflow hidden
        ]


bottomStuff : UniqueClass
bottomStuff =
    uniqueClass
        [ displayFlex
        , property "flex-shrink" "0"
        ]


projectInfo : UniqueClass
projectInfo =
    uniqueClass
        [ padding2 (px 8) zero
        , property "flex-shrink" "0"
        ]


projectInfoTitle : UniqueClass
projectInfoTitle =
    uniqueClass
        [ color (hex Colors.lightGray)
        , textTransform uppercase
        , fontSize (px 14)
        , paddingBottom (px 4)
        , lineHeight (px 14)
        ]


projectInfoInputContainer : UniqueClass
projectInfoInputContainer =
    uniqueClass
        [ position relative
        , overflow hidden
        , marginBottom (px 2)
        , firstChild
            [ borderTopLeftRadius (px 3)
            , borderTopRightRadius (px 3)
            ]
        , lastChild
            [ borderBottomLeftRadius (px 3)
            , borderBottomRightRadius (px 3)
            , marginBottom zero
            ]
        ]


projectInfoInput : UniqueClass
projectInfoInput =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , color (hex Colors.white)
        , display block
        , border zero
        , width (pct 100)
        , outline zero
        , padding3 (px 24) (px 8) (px 16)
        , fontSize (px 16)
        , lineHeight (px 20)
        , fontFamilies [ Constants.sansFont ]
        , fontWeight (int 500)
        ]


projectInfoTextarea : UniqueClass
projectInfoTextarea =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , color (hex Colors.white)
        , display block
        , border zero
        , width (pct 100)
        , outline zero
        , padding3 (px 24) (px 8) (px 16)
        , fontSize (px 16)
        , lineHeight (px 20)
        , property "resize" "none"
        , height (px 100)
        , fontFamilies [ Constants.sansFont ]
        , fontWeight (int 500)
        ]


projectInfoLabel : UniqueClass
projectInfoLabel =
    uniqueClass
        [ position absolute
        , top (px 8)
        , left (px 8)
        , fontSize (px 12)
        , lineHeight (px 12)
        , textTransform uppercase
        , color (hex Colors.lightGray)
        ]


packages : UniqueClass
packages =
    uniqueClass
        [ padding2 (px 8) zero
        , property "flex-shrink" "1"
        , displayFlex
        , flexDirection column
        , overflow hidden
        ]


packagesTitle : UniqueClass
packagesTitle =
    uniqueClass
        [ color (hex Colors.lightGray)
        , textTransform uppercase
        , fontSize (px 14)
        , lineHeight (px 14)
        , paddingBottom (px 4)
        ]


packagesList : UniqueClass
packagesList =
    uniqueClass
        [ overflowY auto
        ]


packagesItem : UniqueClass
packagesItem =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , color (hex Colors.white)
        , width (pct 100)
        , position relative
        , marginBottom (px 2)
        , displayFlex
        , height (px 48)
        , property "justify-content" "space-between"
        , property "align-items" "center"
        , textDecoration none
        , firstChild
            [ borderTopLeftRadius (px 3)
            , borderTopRightRadius (px 3)
            ]
        , lastChild
            [ borderBottomLeftRadius (px 3)
            , borderBottomRightRadius (px 3)
            , marginBottom zero
            ]
        , hover
            [ descendants
                [ Css.class "Views-Editor-Sidebar-packagesItemActions"
                    [ displayFlex
                    ]
                ]
            ]
        ]


packagesItemName : UniqueClass
packagesItemName =
    uniqueClass
        [ fontSize (px 16)
        , padding2 zero (px 12)
        , color (hex Colors.white)
        , whiteSpace noWrap
        , textOverflow ellipsis
        , overflowX hidden
        , overflowY visible
        ]


packagesItemActions : UniqueClass
packagesItemActions =
    uniqueClass
        [ display none
        , property "align-items" "center"
        , height (pct 100)
        ]


packagesItemVersion : UniqueClass
packagesItemVersion =
    uniqueClass
        [ fontSize (px 12)
        , color (hex Colors.lightGray)
        , paddingRight (px 4)
        ]


packagesItemButton : UniqueClass
packagesItemButton =
    uniqueClass
        [ property "background" "none"
        , border zero
        , width (px 48)
        , height (pct 100)
        , display block
        , position relative
        , color (hex Colors.lightGray)
        , cursor pointer
        , padding2 (px 8) zero
        , borderLeft3 (px 2) solid (hex Colors.mediumGray)
        , textDecoration none
        ]


packagesItemButtonInner : UniqueClass
packagesItemButtonInner =
    uniqueClass
        [ displayFlex
        , flexDirection column
        , property "justify-content" "space-between"
        , alignItems center
        , height (pct 100)
        ]


packagesItemButtonIcon : UniqueClass
packagesItemButtonIcon =
    uniqueClass
        [ width (px 20)
        , height (px 20)
        ]


packagesItemButtonText : UniqueClass
packagesItemButtonText =
    uniqueClass
        [ fontSize (px 8)
        , textTransform uppercase
        ]


addPackage : UniqueClass
addPackage =
    uniqueClass
        [ display block
        , backgroundColor (hex Colors.darkGray)
        , color (hex Colors.lightGray)
        , borderRadius (px 3)
        , border zero
        , width (pct 100)
        , padding (px 12)
        , fontSize (px 16)
        , textTransform uppercase
        , fontFamily inherit
        , fontWeight (int 500)
        , cursor pointer
        , marginTop (px 16)
        ]
