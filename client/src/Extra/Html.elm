module Extra.Html exposing (none, viewIf, viewIfLazy)

import Html exposing (Html)


none : Html msg
none =
    Html.text ""


viewIf : Bool -> Html msg -> Html msg
viewIf predicate content =
    if predicate then
        content
    else
        none


viewIfLazy : Bool -> (() -> Html msg) -> Html msg
viewIfLazy predicate thunk =
    if predicate then
        thunk ()
    else
        none
