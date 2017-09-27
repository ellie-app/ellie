module Ellie.Ui.CompileError exposing (view)

import Data.Elm.Compiler.Error as Error exposing (Error)
import Ellie.Ui.CompileError.Styles as Styles
import Html exposing (Html, div, text)
import Shared.Utils as Utils


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
    div [ Styles.container ]
        [ div [ Styles.header ]
            [ div [ Styles.tag ] [ text error.tag ]
            , div [ Styles.location ]
                [ text <| actualLine error ++ ", " ++ actualColumn error
                ]
            ]
        , div
            [ Utils.innerHtml <| Utils.replaceAll error.overview
            , Styles.overview
            ]
            []
        , div
            [ Utils.innerHtml <| Utils.replaceAll error.details
            , Styles.details
            ]
            []
        ]
