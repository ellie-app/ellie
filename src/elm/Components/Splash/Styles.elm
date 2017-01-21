module Components.Splash.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Splash.Classes as Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace Classes.namespace)
        [ (.) Container
            [ width (pct 100)
            , height (pct 100)
            , property "background-image" Colors.pinkPurpleGradient
            , displayFlex
            , property "justify-content" "center"
            , alignItems center
            , flexDirection column
            ]
        , (.) MainText
            [ fontFamilies [ Constants.scriptFont ]
            , fontSize (px 64)
            , margin zero
            , color (hex Colors.white)
            , marginBottom (px 32)
            ]
        , (.) SubText
            [ fontFamilies [ Constants.sansFont ]
            , fontSize (px 36)
            , textAlign center
            , color (hex Colors.white)
            , margin zero
            ]
        ]
