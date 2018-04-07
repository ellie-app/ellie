module Ellie.Ui.CopyText exposing (view)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)


view : String -> Html msg
view contents =
    Html.node "ellie-ui-copy-text"
        [ css
            [ backgroundColor Theme.primaryBackground
            , displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , position relative
            ]
        ]
        [ Html.node "ellie-ui-copy-text-content"
            [ css
                [ color Theme.secondaryForeground
                , fontFamily monospace
                , fontSize (px 14)
                , width (pct 100)
                , padding (px 12)
                , property "word-break" "break-word"
                ]
            ]
            [ Html.text contents
            ]
        , Html.node "ellie-ui-copy-text-note"
            [ css
                [ position absolute
                , right (px 40)
                , top (pct 50)
                , transform <| translateY (pct -50)
                , backgroundColor Theme.success
                , color Theme.primaryForeground
                , fontSize (px 12)
                , textTransform uppercase
                , padding (px 4)
                , visibility hidden
                , lineHeight (num 1)
                ]
            ]
            []
        , Html.button
            [ css
                [ property "background" "none"
                , border zero
                , outline zero
                , cursor pointer
                , padding (px 12)
                , height (px 38)
                , width (px 38)
                , color Theme.secondaryForeground
                , hover [ color Theme.primaryForeground ]
                , active [ transform <| scale 1.2 ]
                , flexShrink zero
                ]
            ]
            [ Icon.view Icon.Copy
            ]
        ]
