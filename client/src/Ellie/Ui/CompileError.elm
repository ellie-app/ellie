module Ellie.Ui.CompileError exposing (view)

import Colors
import Css exposing (..)
import Data.Elm.Compiler.Error as Error exposing (Error)
import Html.Styled exposing (Html, Attribute, fromUnstyled, div, text)
import Html.Styled.Attributes exposing (css)
import Markdown


actualLine : Error -> String
actualLine error =
    error.subregion
        |> Maybe.withDefault error.region
        |> (.start >> .line)
        |> toString
        |> (++) "Line "


actualColumn : Error -> String
actualColumn error =
    error.subregion
        |> Maybe.withDefault error.region
        |> (.start >> .column)
        |> toString
        |> (++) "Column "


view : Error -> Html msg
view error =
    div [ containerStyles ]
        [ div [ headerStyles ]
            [ div [ tagStyles ] [ text error.tag ]
            , div [ locationStyles ] [ text <| actualLine error ++ ", " ++ actualColumn error ]
            ]
        , div [ overviewStyles ] <| List.map fromUnstyled (Markdown.toHtml Nothing error.overview)
        , div [ detailsStyles ] <| List.map fromUnstyled (Markdown.toHtml Nothing error.details)
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ width (pct 100)
        , backgroundColor Colors.darkMediumGray
        , Colors.boxShadow |> .bottom
        , padding2 (px 12) (px 16)
        , borderLeft3 (px 1) solid Colors.red
        , color Colors.lightGray
        , position relative
        ]


headerStyles : Attribute msg
headerStyles =
    css
        [ displayFlex
        , justifyContent spaceBetween
        , paddingBottom (px 12)
        ]


tagStyles : Attribute msg
tagStyles =
    css
        [ fontSize (px 16)
        , fontWeight bold
        ]


locationStyles : Attribute msg
locationStyles =
    css
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        ]


overviewStyles : Attribute msg
overviewStyles =
    css
        [ fontSize (px 16)
        , paddingBottom (px 16)
        , lineHeight (px 16)
        , color Colors.lightMediumGray
        ]


detailsStyles : Attribute msg
detailsStyles =
    css
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        ]
