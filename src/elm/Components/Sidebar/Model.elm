module Components.Sidebar.Model
    exposing
        ( Model
        , SearchFlow(..)
        , model
        , toggleDetails
        , togglePackages
        , startNewPackageSearch
        , cancelSearch
        )

import Types.Dependency as Dependency exposing (Dependency)
import Components.PackageSearch.Model as PackageSearch


type SearchFlow
    = NotSearching
    | NewPackageSearching PackageSearch.Model


type alias Model =
    { detailsOpen : Bool
    , packagesOpen : Bool
    , searchFlow : SearchFlow
    }


model : Model
model =
    { detailsOpen = True
    , packagesOpen = True
    , searchFlow = NotSearching
    }


toggleDetails : Model -> Model
toggleDetails model =
    { model | detailsOpen = not model.detailsOpen }


togglePackages : Model -> Model
togglePackages model =
    { model | packagesOpen = not model.packagesOpen }


startNewPackageSearch : Model -> Model
startNewPackageSearch model =
    { model | searchFlow = NewPackageSearching PackageSearch.model }


cancelSearch : Model -> Model
cancelSearch model =
    { model | searchFlow = NotSearching }
