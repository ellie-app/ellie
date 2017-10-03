module Views.Editor.Ad exposing (view)

import Html exposing (Html, div, node, text)
import Html.Attributes exposing (async, id, src, type_)
import Shared.Constants as Constants


view : Html msg
view =
    if Constants.isProduction then
        div [ id "carbon" ]
            [ node "script"
                [ id "_carbonads_js"
                , type_ "text/javascript"
                , async True
                , "//cdn.carbonads.com/carbon.js?zoneid="
                    ++ Constants.carbonZoneId
                    ++ "&serve="
                    ++ Constants.carbonServe
                    ++ "&placement="
                    ++ Constants.carbonPlacement
                    |> src
                ]
                []
            ]
    else
        text ""
