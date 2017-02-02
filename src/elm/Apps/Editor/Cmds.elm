port module Apps.Editor.Cmds
    exposing
        ( withCmd
        , withAdditionalCmd
        , compile
        , writeIframe
        , notifyIframeResult
        , reloadIframe
        )

import RemoteData exposing (RemoteData(..))
import Types.CompileError as CompileError exposing (CompileError)
import Types.ApiError as ApiError exposing (ApiError)
import Types.Notification as Notification exposing (Notification)
import Shared.MessageBus as MessageBus
import Shared.Api as Api
import Apps.Editor.Model as Model exposing (Model)


withCmd : (Model -> Cmd msg) -> Model -> ( Model, Cmd msg )
withCmd makeCmd model =
    ( model, makeCmd model )


withAdditionalCmd : (Model -> Cmd msg) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
withAdditionalCmd makeCmd ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , makeCmd model
        ]
    )


port reloadIframePort : () -> Cmd msg


reloadIframe : Cmd msg
reloadIframe =
    reloadIframePort ()


compile : (Result ApiError (List CompileError) -> msg) -> Model -> Cmd msg
compile completeMsg model =
    case ( model.session, Model.shouldCompileElm model ) of
        ( Success session, True ) ->
            Cmd.batch
                [ Api.compile model.stagedElmCode session
                    |> Api.send completeMsg
                , MessageBus.notify
                    Notification.Info
                    "Compilation Started"
                    "Ellie is compiling your code."
                ]

        _ ->
            Cmd.none


writeIframe : Bool -> (Result ApiError () -> msg) -> Model -> Cmd msg
writeIframe force completeMsg model =
    case ( model.session, force || Model.shouldWriteIframe model ) of
        ( Success session, True ) ->
            Cmd.batch
                [ Api.writeIframe model.stagedHtmlCode session
                    |> Api.send completeMsg
                , MessageBus.notify
                    Notification.Info
                    "IFrame Generation Started"
                    "Ellie is building your HTML output. When it's done we'll show you the result below."
                ]

        _ ->
            Cmd.none


notifyIframeResult : Result ApiError () -> Model -> Cmd msg
notifyIframeResult result model =
    case result of
        Ok _ ->
            MessageBus.notify
                Notification.Success
                "IFrame Generation Succeeded"
                "Ellie wrote your HTML output without any problems. Awesome!"

        Err apiError ->
            MessageBus.notify
                Notification.Error
                "IFrame Generation Failed"
                ("Ellie couldn't write your HTML output. Here's what the server said:\n\n" ++ apiError.explanation)
