module Views.Editor.Sidebar.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Shared.Constants as Constants
import Views.Editor.Sidebar.Classes exposing (..)


styles : Stylesheet
styles =
    (stylesheet << namespace "components_sidebar_")
        [ Css.class Sidebar
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
        , Css.class TopStuff
            [ displayFlex
            , flexDirection column
            , property "flex-shrink" "1"
            , overflow hidden
            ]
        , Css.class BottomStuff
            [ displayFlex
            , property "flex-shrink" "0"
            ]
        , Css.class ProjectInfo
            [ padding2 (px 8) zero
            , property "flex-shrink" "0"
            ]
        , Css.class ProjectInfoTitle
            [ color (hex Colors.lightGray)
            , textTransform uppercase
            , fontSize (px 14)
            , paddingBottom (px 4)
            , lineHeight (px 14)
            ]
        , Css.class ProjectInfoInputContainer
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
        , Css.class ProjectInfoInput
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
        , Css.class ProjectInfoTextarea
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
        , Css.class ProjectInfoLabel
            [ position absolute
            , top (px 8)
            , left (px 8)
            , fontSize (px 12)
            , lineHeight (px 12)
            , textTransform uppercase
            , color (hex Colors.lightGray)
            ]
        , Css.class Packages
            [ padding2 (px 8) zero
            , property "flex-shrink" "1"
            , displayFlex
            , flexDirection column
            , overflow hidden
            ]
        , Css.class PackagesTitle
            [ color (hex Colors.lightGray)
            , textTransform uppercase
            , fontSize (px 14)
            , lineHeight (px 14)
            , paddingBottom (px 4)
            ]
        , Css.class PackagesList
            [ overflowY auto
            ]
        , Css.class PackagesItem
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
                    [ Css.class PackagesItemActions
                        [ displayFlex
                        ]
                    ]
                ]
            ]
        , Css.class PackagesItemName
            [ fontSize (px 16)
            , padding2 zero (px 12)
            , color (hex Colors.white)
            , whiteSpace noWrap
            , textOverflow ellipsis
            , overflowX hidden
            , overflowY visible
            ]
        , Css.class PackagesItemActions
            [ display none
            , property "align-items" "center"
            , height (pct 100)
            ]
        , Css.class PackagesItemVersion
            [ fontSize (px 12)
            , color (hex Colors.lightGray)
            , paddingRight (px 4)
            ]
        , Css.class PackagesItemButton
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
        , Css.class PackagesItemButtonInner
            [ displayFlex
            , flexDirection column
            , property "justify-content" "space-between"
            , alignItems center
            , height (pct 100)
            ]
        , Css.class PackagesItemButtonIcon
            [ width (px 20)
            , height (px 20)
            ]
        , Css.class PackagesItemButtonText
            [ fontSize (px 8)
            , textTransform uppercase
            ]
        , Css.class AddPackage
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
        ]
