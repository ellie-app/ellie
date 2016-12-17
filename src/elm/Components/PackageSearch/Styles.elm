module Components.PackageSearch.Styles exposing (styles)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.PackageSearch.Classes exposing (..)
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "components_packageSearch_")
        [ (.) ActionIcon
            [ width (px 20)
            , height (px 20)
            , display inlineBlock
            ]
        ]
