module Pages.Editor.Header.Model exposing (Model, model)


type alias Model =
    { shareOpen : Bool }


model : Model
model =
    { shareOpen = False }
