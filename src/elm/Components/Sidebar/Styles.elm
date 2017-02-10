module Components.Sidebar.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Sidebar.Classes exposing (..)
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "components_sidebar_")
        [ (.) Sidebar
            [ height (pct 100)
            , width (px Constants.sidebarWidth)
            , backgroundColor (hex Colors.mediumGray)
            , padding2 (px 8) (px 16)
            , property "box-shadow" "2px 0 8px 2px rgba(0,0,0,0.50)"
            , position relative
            , property "z-index" "1"
            ]
        , (.) ProjectInfo
            [ padding2 (px 8) zero
            ]
        , (.) ProjectInfoTitle
            [ color (hex Colors.lightGray)
            , textTransform uppercase
            , fontSize (px 14)
            , paddingBottom (px 4)
            , lineHeight (px 14)
            ]
        , (.) ProjectInfoInputContainer
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
        , (.) ProjectInfoInput
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
        , (.) ProjectInfoTextarea
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
            , height (px 120)
            , fontFamilies [ Constants.sansFont ]
            , fontWeight (int 500)
            ]
        , (.) ProjectInfoLabel
            [ position absolute
            , top (px 8)
            , left (px 8)
            , fontSize (px 12)
            , lineHeight (px 12)
            , textTransform uppercase
            , color (hex Colors.lightGray)
            ]
        , (.) Packages
            [ padding2 (px 8) zero
            ]
        , (.) PackagesTitle
            [ color (hex Colors.lightGray)
            , textTransform uppercase
            , fontSize (px 14)
            , lineHeight (px 14)
            , paddingBottom (px 4)
            ]
        , (.) PackagesList
            [ maxHeight (px 376)
            , overflowY auto
            ]
        , (.) PackagesItem
            [ backgroundColor (hex Colors.darkGray)
            , color (hex Colors.white)
            , width (pct 100)
            , position relative
            , marginBottom (px 2)
            , displayFlex
            , property "justify-content" "space-between"
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
        , (.) PackagesItemInfo
            [ fontSize (px 12)
            , lineHeight (px 12)
            , padding2 (px 12) (px 8)
            , property "width" "calc(100% - 48px)"
            , borderRight3 (px 2) solid (hex Colors.mediumGray)
            ]
        , (.) PackagesItemInfoName
            [ color (hex Colors.lightGray)
            , paddingBottom (px 8)
            ]
        , (.) PackagesItemInfoNameUsername
            [ color (hex Colors.mediumGray)
            ]
        , (.) PackagesItemInfoVersion
            [ color (hex Colors.lightGray)
            ]
        , (.) PackagesItemRemove
            [ property "background" "none"
            , border zero
            , width (px 48)
            , display block
            , position relative
            , color (hex Colors.lightGray)
            , cursor pointer
            ]
        , (.) PackagesItemRemoveInner
            [ displayFlex
            , flexDirection column
            , property "justify-content" "center"
            , alignItems center
            ]
        , (.) PackagesItemRemoveIcon
            [ width (px 24)
            , height (px 24)
            , paddingBottom (px 4)
            ]
        , (.) PackagesItemRemoveText
            [ fontSize (px 8)
            , textTransform uppercase
            ]
        , (.) AddPackage
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
        , (.) Loading
            [ padding2 (px 12) (px 16)
            , backgroundColor (hex Colors.darkGray)
            , displayFlex
            , property "justify-content" "center"
            , alignItems center
            , flexDirection column
            , borderRadius (px 3)
            , marginTop (px 16)
            ]
        , (.) LoadingPackageInfo
            [ fontSize (px 12)
            , color (hex Colors.lightGray)
            , paddingBottom (px 8)
            ]
        , (.) LoadingAnimContainer
            [ width (px 50)
            , position relative
            , height (px 20)
            , color (hex Colors.mediumGray)
            ]
        , (.) LoadingAnim
            []
        ]
