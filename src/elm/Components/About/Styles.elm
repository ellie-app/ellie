module Components.About.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.About.Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "components_about_")
        [ (.) Popout
            [ position relative
            , backgroundColor (hex Colors.mediumGray)
            , width (px 400)
            , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
            , property "z-index" "4"
            , borderRadius (px 3)
            , padding (px 16)
            , overflowY auto
            , border3 (px 2) solid (hex Colors.lightGray)
            , color (hex Colors.white)
            ]
        , (.) Title
            [ fontSize (px 18)
            , fontWeight (int 700)
            ]
        , (.) Paragraph
            [ fontSize (px 16)
            ]
        , (.) Link
            [ color inherit
            , textDecoration none
            , fontWeight (int 700)
            ]
        , (.) Creators
            [ paddingTop (px 12)
            , borderTop3 (px 2) solid (hex Colors.white)
            ]
        , (.) CreatorLine
            [ textAlign center
            ]
        , (.) Logo
            [ fontFamilies [ Constants.scriptFont ]
            , fontSize (px 20)
            ]
        , (.) ImagesAndHsLogo
            [ displayFlex
            , property "justify-content" "space-between"
            , alignItems center
            , paddingTop (px 12)
            ]
        , (.) PartnerImageContainer
            [ displayFlex
            , flexDirection column
            , alignItems center
            , textDecoration none
            , color inherit
            ]
        , (.) PartnerImage
            [ width (px 48)
            , height (px 48)
            , borderRadius (pct 50)
            ]
        , (.) HsLogoImage
            [ width (px 170)
            , height (px 45)
            , backgroundImage (url <| Constants.asset "images/humblespark_logo.png")
            , backgroundSize (pct 102)
            , marginTop (px -16)
            ]
        ]
