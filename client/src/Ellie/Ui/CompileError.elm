module Ellie.Ui.CompileError exposing (view)

import Data.Elm.Compiler.Error as Error exposing (Error)
import Html exposing (Html, div, text)


view : Error -> Html msg
view error =
    div []
        [ text "hi" ]
