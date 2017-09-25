module Pages.Editor.Sidebar.Update exposing (..)

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Pages.Editor.Sidebar.Model as Model exposing (Model)
import Shared.Api as Api


type Msg
    = SearchChanged String
    | ResultsLoaded (Result ApiError (List Package))
    | ChangePanel Model.Panel


update : Version -> Msg -> Model -> ( Model, Cmd Msg )
update version msg model =
    case msg of
        SearchChanged search ->
            if String.length search > 3 then
                ( { model | search = search }
                , Api.searchPackages version search
                    |> Api.send ResultsLoaded
                )
            else
                ( { model | search = search, results = [] }
                , Cmd.none
                )

        ResultsLoaded result ->
            ( { model | results = Result.withDefault [] result }
            , Cmd.none
            )

        ChangePanel panel ->
            ( { model | panel = panel }
            , Cmd.none
            )
