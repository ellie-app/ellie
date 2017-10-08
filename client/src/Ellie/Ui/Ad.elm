module Ellie.Ui.Ad exposing (view)

import Ellie.Ui.Ad.Styles as Styles
import Extra.Html.Attributes exposing (style)
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (async, id, src, type_)
import Shared.Constants as Constants


type alias Config =
    { zoneId : String
    , serve : String
    , placement : String
    }


view : Config -> Html msg
view { zoneId, serve, placement } =
    div [ Styles.container ]
        [ if Constants.isProduction then
            div [ id "carbon" ]
                [ node "script"
                    [ id "_carbonads_js"
                    , type_ "text/javascript"
                    , async True
                    , "//cdn.carbonads.com/carbon.js?zoneid="
                        ++ zoneId
                        ++ "&serve="
                        ++ serve
                        ++ "&placement="
                        ++ placement
                        |> src
                    ]
                    []
                ]
          else
            div [ style "height" "140px" ] []
        ]
