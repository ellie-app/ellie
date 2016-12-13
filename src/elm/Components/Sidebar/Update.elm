module Components.Sidebar.Update exposing (Msg(..), initialize, update)

import Components.Sidebar.Model as Model exposing (Model)


type Msg
    = NoOp
    | DetailsToggled
    | PackagesToggled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DetailsToggled ->
            ( { model | detailsOpen = not model.detailsOpen }
            , Cmd.none
            )

        PackagesToggled ->
            ( { model | packagesOpen = not model.packagesOpen }
            , Cmd.none
            )


initialize : Cmd Msg
initialize =
    Cmd.none
