module Pages.Editor.Header.Update exposing (..)

import Pages.Editor.Header.Model exposing (Model)


type Msg
    = ToggleShare Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleShare open ->
            { model | shareOpen = open }
