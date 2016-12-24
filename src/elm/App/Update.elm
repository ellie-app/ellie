module App.Update exposing (update, initialize, onRouteChange, Msg(..))

import Window exposing (Size)
import Mouse exposing (Position)
import RemoteData exposing (RemoteData(..))
import Navigation
import Types.ApiError as ApiError exposing (ApiError)
import Types.Revision as Revision exposing (Revision)
import Types.ExistingRevision as ExistingRevision exposing (ExistingRevision)
import Types.Session as Session exposing (Session)
import Types.CompileError as CompileError exposing (CompileError)
import App.Model as Model exposing (Model)
import App.Routing as Routing exposing (Route(..))
import Components.Sidebar.Update as Sidebar
import Shared.Api as Api
import Shared.Constants as Constants
import Shared.Utils as Utils


resultIsSuccess : Result x a -> Bool
resultIsSuccess result =
    result
        |> Result.map (\_ -> True)
        |> Result.withDefault False


boolToMaybe : Bool -> Maybe ()
boolToMaybe bool =
    if bool then
        Just ()
    else
        Nothing



-- UPDATE


type Msg
    = CreateSessionCompleted (Result ApiError Session)
    | RouteChanged Route
    | LoadRevisionCompleted (Result ApiError Revision)
    | CompileRequested
    | CompileCompleted (Result ApiError (List CompileError))
    | ElmCodeChanged String
    | HtmlCodeChanged String
    | SaveRequested
    | SaveCompleted (Result ApiError Revision)
    | OnlineChanged Bool
    | FormattingRequested
    | FormattingCompleted (Result ApiError String)
    | NoOp


saveProject : Model -> Cmd Msg
saveProject model =
    if Model.isSavedProject model && Model.isOwnedProject model then
        model.clientRevision
            |> (\r -> { r | revisionNumber = Maybe.map ((+) 1) r.revisionNumber })
            |> Api.createRevision
            |> Api.send SaveCompleted
    else
        model.clientRevision
            |> Api.createProjectFromRevision
            |> Api.send SaveCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateSessionCompleted sessionResult ->
            ( { model | session = RemoteData.fromResult sessionResult }
            , Cmd.none
            )

        LoadRevisionCompleted revisionResult ->
            ( { model
                | serverRevision = RemoteData.fromResult revisionResult
                , clientRevision = Result.withDefault model.clientRevision revisionResult
              }
            , Cmd.none
            )

        RouteChanged route ->
            handleRouteChanged route ( { model | currentRoute = route }, Cmd.none )

        CompileRequested ->
            ( { model | compileResult = Loading }
            , model.session
                |> RemoteData.map (Api.compile model.clientRevision.elmCode)
                |> RemoteData.map (Api.send CompileCompleted)
                |> RemoteData.withDefault Cmd.none
            )

        CompileCompleted compileResult ->
            ( { model
                | compileResult =
                    RemoteData.fromResult compileResult
                , firstCompileComplete =
                    model.firstCompileComplete
                        || resultIsSuccess compileResult
                , elmCodeChanged =
                    not (resultIsSuccess compileResult)
              }
            , Cmd.none
            )

        ElmCodeChanged code ->
            ( model
                |> Model.updateClientRevision (\r -> { r | elmCode = code })
                |> (\m -> { m | elmCodeChanged = True })
            , Cmd.none
            )

        HtmlCodeChanged code ->
            ( model
                |> Model.updateClientRevision (\r -> { r | htmlCode = code })
            , Cmd.none
            )

        SaveRequested ->
            ( { model | saveState = Loading }
            , saveProject model
            )

        SaveCompleted saveResult ->
            ( { model
                | saveState =
                    saveResult
                        |> Result.map (\_ -> ())
                        |> RemoteData.fromResult
                , serverRevision =
                    RemoteData.fromResult saveResult
                , clientRevision =
                    Result.withDefault model.clientRevision saveResult
              }
            , saveResult
                |> Result.toMaybe
                |> Maybe.andThen (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                |> Maybe.map (\( p, r ) -> Routing.construct <| SpecificRevision p r)
                |> Maybe.map Navigation.modifyUrl
                |> Maybe.withDefault Cmd.none
            )

        OnlineChanged isOnline ->
            ( { model | isOnline = isOnline }
            , Cmd.none
            )

        FormattingRequested ->
            ( model
            , Api.format model.clientRevision.elmCode
                |> Api.send FormattingCompleted
            )

        FormattingCompleted result ->
            ( model
                |> Model.updateClientRevision (\r -> { r | elmCode = Result.withDefault r.elmCode result })
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


onRouteChange : Navigation.Location -> Msg
onRouteChange =
    Routing.parse >> RouteChanged


initialize : Navigation.Location -> ( Model, Cmd Msg )
initialize location =
    let
        initialModel =
            Model.model
    in
        location
            |> Routing.parse
            |> (\route -> { initialModel | currentRoute = route })
            |> (\model -> ( model, Cmd.none ))
            |> (\( model, cmd ) -> handleRouteChanged model.currentRoute ( model, cmd ))


handleRouteChanged : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRouteChanged route ( model, cmd ) =
    ( case route of
        NotFound ->
            model

        NewProject ->
            model.session
                |> RemoteData.map (\_ -> model)
                |> RemoteData.withDefault { model | session = Loading, serverRevision = Loading }

        SpecificRevision projectId revisionNumber ->
            let
                modelForSession =
                    model.session
                        |> RemoteData.map (\_ -> model)
                        |> RemoteData.withDefault { model | session = Loading }
            in
                model.clientRevision
                    |> (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                    |> Maybe.map (\( p, r ) -> p /= projectId || r /= revisionNumber)
                    |> Maybe.andThen boolToMaybe
                    |> Maybe.map (\() -> { modelForSession | serverRevision = Loading })
                    |> Maybe.withDefault modelForSession

        _ ->
            model
    , Cmd.batch
        [ cmd
        , case route of
            NotFound ->
                Navigation.modifyUrl (Routing.construct NewProject)

            NewProject ->
                Cmd.batch
                    [ model.session
                        |> RemoteData.map (\_ -> Cmd.none)
                        |> RemoteData.withDefault (Api.createSession |> Api.send CreateSessionCompleted)
                    , Api.defaultRevision
                        |> Api.send LoadRevisionCompleted
                    ]

            SpecificRevision projectId revisionNumber ->
                Cmd.batch
                    [ model.session
                        |> RemoteData.map (\_ -> Cmd.none)
                        |> RemoteData.withDefault (Api.createSession |> Api.send CreateSessionCompleted)
                    , model.clientRevision
                        |> (\r -> Maybe.map2 (,) r.projectId r.revisionNumber)
                        |> Maybe.map (\( p, r ) -> p /= projectId || r /= revisionNumber)
                        |> Maybe.withDefault True
                        |> boolToMaybe
                        |> Maybe.map (\() -> Api.exactRevision projectId revisionNumber)
                        |> Maybe.map (Api.send LoadRevisionCompleted)
                        |> Maybe.withDefault Cmd.none
                    ]

            _ ->
                Cmd.none
        ]
    )
