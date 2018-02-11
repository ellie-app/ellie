module Ellie.Ui.SplitPane exposing (..)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html, node)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode


type Direction
    = Vertical
    | Horizontal


type alias Config msg =
    { direction : Direction
    , ratio : Float
    , onResize : Float -> msg
    , first : Html msg
    , second : Html msg
    }


view : Config msg -> Html msg
view { direction, ratio, onResize, first, second } =
    let
        attrs =
            case direction of
                Vertical ->
                    [ verticalStyles
                    , Attributes.property "isVertical" <| Encode.bool True
                    , Attributes.property "ratio" <| Encode.float ratio
                    , Events.on "resize" <| Decode.map onResize <| Decode.at [ "target", "ratio" ] Decode.float
                    ]

                Horizontal ->
                    [ horizontalStyles
                    , Attributes.property "isVertical" <| Encode.bool False
                    , Attributes.property "ratio" <| Encode.float ratio
                    , Events.on "resize" <| Decode.map onResize <| Decode.at [ "target", "ratio" ] Decode.float
                    ]
    in
    Html.node "ellie-ui-split-pane-group"
        attrs
        [ Html.node "ellie-ui-split-pane-panel" [] [ first ]
        , Html.node "ellie-ui-split-pane-divider" [] []
        , Html.node "ellie-ui-split-pane-panel" [] [ second ]
        ]


horizontalStyles : Attribute msg
horizontalStyles =
    css
        [ displayFlex
        , flexDirection row
        , position relative
        , width (pct 100)
        , height (pct 100)
        , Css.Foreign.children
            [ Css.Foreign.typeSelector "ellie-ui-split-pane-panel"
                [ width (pct 50)
                , height (pct 100)
                , firstChild
                    [ borderRight3 (px 1) solid Theme.draggableBorder
                    ]
                , lastChild
                    [ borderLeft3 (px 1) solid Theme.draggableBorder
                    ]
                ]
            , Css.Foreign.typeSelector "ellie-ui-split-pane-divider"
                [ width (px 10)
                , marginLeft (px -5)
                , position absolute
                , top zero
                , left (pct 50)
                , cursor ewResize
                , zIndex (int 1)
                , height (pct 100)
                ]
            ]
        ]


verticalStyles : Attribute msg
verticalStyles =
    css
        [ displayFlex
        , flexDirection column
        , position relative
        , width (pct 100)
        , height (pct 100)
        , Css.Foreign.children
            [ Css.Foreign.typeSelector "ellie-ui-split-pane-panel"
                [ height (pct 50)
                , width (pct 100)
                , firstChild
                    [ borderBottom3 (px 1) solid Theme.draggableBorder
                    ]
                , lastChild
                    [ borderTop3 (px 1) solid Theme.draggableBorder
                    ]
                ]
            , Css.Foreign.typeSelector "ellie-ui-split-pane-divider"
                [ height (px 10)
                , marginTop (px -5)
                , position absolute
                , left zero
                , top (pct 50)
                , cursor nsResize
                , zIndex (int 1)
                , width (pct 100)
                ]
            ]
        ]
