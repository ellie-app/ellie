module Components.Sidebar.Model exposing (Model, model)


type alias Model =
    { detailsOpen : Bool
    , packagesOpen : Bool
    }


model : Model
model =
    { detailsOpen = True
    , packagesOpen = True
    }
