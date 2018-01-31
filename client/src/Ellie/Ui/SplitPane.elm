module Ellie.Ui.SplitPane exposing (..)

import Colors
import Css exposing (..)
import Css.Foreign
import Html.Styled as Html exposing (Html, node)
import Html.Styled.Attributes as Attributes exposing (css)


view : Html msg -> Html msg -> Html msg
view leftPanel rightPanel =
    Html.node "ellie-ui-split-pane-group"
        [ css
            [ displayFlex
            , flexDirection row
            , position relative
            , width (pct 100)
            , height (pct 100)
            , Css.Foreign.children
                [ Css.Foreign.typeSelector "ellie-ui-split-pane-panel"
                    [ width (pct 50)
                    , height (pct 100)
                    ]
                , Css.Foreign.typeSelector "ellie-ui-split-pane-divider"
                    [ width (px 4)
                    , marginLeft (px -2)
                    , position absolute
                    , top zero
                    , left (pct 50)
                    , cursor ewResize
                    , zIndex (int 1)
                    , height (pct 100)
                    , backgroundColor Colors.mediumGray
                    ]
                ]
            ]
        ]
        [ Html.node "ellie-ui-split-pane-panel" [] [ leftPanel ]
        , Html.node "ellie-ui-split-pane-divider" [] []
        , Html.node "ellie-ui-split-pane-panel" [] [ rightPanel ]
        ]
