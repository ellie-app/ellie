module Pages.Editor.Save.Update exposing (..)

import Data.Aws.UploadSignature as UploadSignature exposing (UploadSignature)
import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Notification as Notification exposing (Notification)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Ellie.SaveState as SaveState exposing (SaveState)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Compiler.Error as CompilerError
import Http.Extra as Http
import Json.Encode as Encode exposing (Value)
import Navigation
import Pages.Editor.Cmds as Cmds
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Routing as Routing
import RemoteData
import Shared.Api as Api
import Shared.Aws as Aws
import Shared.Opbeat as Opbeat
import Task


type Msg
    = Start
    | TermsAcceptanceStart TermsVersion
    | TermsAcceptanceComplete (Result ApiError Http.NoContent)
    | CompileStarted Int
    | CompileCompleted (Result (List CompilerError.Error) String)
    | UploadSignaturesAcquired (Result ApiError ( UploadSignature, UploadSignature ))
    | CompileAborted String
    | UploadSucceeded String
    | UploadFailed String String
    | ReportException Opbeat.Exception


type alias NotificationInfo =
    { title : String
    , level : Notification.Level
    , message : String
    }


update : Model -> Msg -> ( Model, Maybe NotificationInfo, Cmd Msg )
update model msg =
    case msg of
        Start ->
            if
                SaveState.canSave model.saveState
                    && (model.acceptedTermsVersion == Just model.latestTermsVersion)
            then
                { model | saveState = SaveState.Compiling }
                    |> Model.commitStagedCode
                    |> (\m -> ( m, Nothing, Cmds.compile m True ))
            else
                ( { model
                    | termsShown = True
                    , saveState = SaveState.AwaitingTermsAcceptance
                  }
                , Nothing
                , Cmd.none
                )

        TermsAcceptanceStart termsVersion ->
            ( { model | saveState = SaveState.AcceptingTerms }
            , Nothing
            , Api.acceptTerms termsVersion
                |> Api.send TermsAcceptanceComplete
            )

        TermsAcceptanceComplete (Ok _) ->
            { model
                | acceptedTermsVersion = Just model.latestTermsVersion
            }
                |> Model.commitStagedCode
                |> (\m -> ( m, Nothing, Cmds.compile model True ))

        TermsAcceptanceComplete (Err apiError) ->
            ( { model
                | saveState =
                    apiError
                        |> SaveState.TermsAcceptanceFailed
                        |> SaveState.Failed
              }
            , Just <|
                NotificationInfo
                    "Accepting Terms Failed"
                    Notification.Error
                    "Something went wrong while accepting our terms. This isn't supposed to happen, and we're working on fixing it! You can also try again."
            , Cmd.none
            )

        CompileStarted totalModules ->
            ( { model | saveState = SaveState.Compiling }
            , if totalModules >= 5 then
                Just <|
                    NotificationInfo
                        "Saving may take a while"
                        Notification.Info
                        "It looks like there are a lot of modules to compile. Please wait a moment while I build everything and save it!"
              else
                Nothing
            , Cmd.none
            )

        CompileAborted message ->
            ( { model
                | saveState =
                    message
                        |> SaveState.CompileFailed
                        |> SaveState.Failed
              }
            , Just <|
                NotificationInfo
                    "Compiling Failed"
                    Notification.Error
                    message
            , Cmd.none
            )

        CompileCompleted result ->
            ( { model | saveState = SaveState.RequestingSignature result }
            , Nothing
            , Api.uploadSignatures model.clientRevision
                |> Task.attempt UploadSignaturesAcquired
            )

        UploadSignaturesAcquired (Err apiError) ->
            ( { model
                | saveState =
                    apiError
                        |> SaveState.SignatureFailed
                        |> SaveState.Failed
              }
            , Just <|
                NotificationInfo
                    "Failed To Save Project"
                    Notification.Error
                    ("Ellie couldn't save your project. Here's what the server said:\n" ++ apiError.explanation)
            , Cmd.none
            )

        UploadSignaturesAcquired (Ok ( revisionSig, resultSig )) ->
            case ( model.acceptedTermsVersion, model.saveState ) of
                ( Just termsVersion, SaveState.RequestingSignature (Ok resultUrl) ) ->
                    let
                        currentRevision =
                            model.clientRevision

                        updatedRevision =
                            { currentRevision
                                | snapshot = Revision.Uploaded
                                , id = Just <| RevisionId revisionSig.projectId revisionSig.revisionNumber
                                , acceptedTerms = Just termsVersion
                                , owned = True
                            }
                    in
                    ( { model | saveState = SaveState.Uploading updatedRevision }
                    , Nothing
                    , Aws.uploadBatch (revisionSig.projectId ++ "-" ++ toString revisionSig.revisionNumber)
                        [ { name = "revision.json"
                          , mime = "application/json"
                          , url = revisionSig.url
                          , fields = revisionSig.fields
                          , content =
                                updatedRevision
                                    |> Revision.encoder
                                    |> Encode.encode 0
                                    |> Aws.Direct
                          }
                        , { name = "result.html"
                          , mime = "text/html"
                          , url = resultSig.url
                          , fields = resultSig.fields
                          , content = Aws.Stream resultUrl
                          }
                        ]
                    )

                ( Just termsVersion, SaveState.RequestingSignature (Err errors) ) ->
                    let
                        currentRevision =
                            model.clientRevision

                        updatedRevision =
                            { currentRevision
                                | snapshot = Revision.Errored errors
                                , id = Just <| RevisionId revisionSig.projectId revisionSig.revisionNumber
                                , acceptedTerms = Just termsVersion
                                , owned = True
                            }
                    in
                    ( { model | saveState = SaveState.Uploading updatedRevision }
                    , Nothing
                    , Aws.upload (revisionSig.projectId ++ "-" ++ toString revisionSig.revisionNumber)
                        { name = "revision.json"
                        , mime = "application/json"
                        , url = revisionSig.url
                        , fields = revisionSig.fields
                        , content =
                            updatedRevision
                                |> Revision.encoder
                                |> Encode.encode 0
                                |> Aws.Direct
                        }
                    )

                _ ->
                    ( model, Nothing, Cmd.none )

        UploadSucceeded id ->
            case model.saveState of
                SaveState.Uploading revision ->
                    case revision.id of
                        Just { projectId, revisionNumber } ->
                            if id == (projectId ++ "-" ++ toString revisionNumber) then
                                ( { model
                                    | saveState = SaveState.Ready
                                    , serverRevision = RemoteData.Success revision
                                    , clientRevision = revision
                                  }
                                    |> Model.resetStagedCode
                                , Just <|
                                    NotificationInfo
                                        "Your Project Was Saved"
                                        Notification.Success
                                        "Ellie saved your project! Your revision number has been updated in the URL."
                                , revision.id
                                    |> Maybe.map Routing.SpecificRevision
                                    |> Maybe.map (Routing.construct >> Navigation.newUrl)
                                    |> Maybe.withDefault Cmd.none
                                )
                            else
                                ( model, Nothing, Cmd.none )

                        Nothing ->
                            ( model, Nothing, Cmd.none )

                _ ->
                    ( model, Nothing, Cmd.none )

        UploadFailed id message ->
            ( { model
                | saveState =
                    message
                        |> SaveState.UploadFailed
                        |> SaveState.Failed
              }
            , Just <|
                NotificationInfo
                    "Failed To Save Project"
                    Notification.Error
                    ("Ellie couldn't save your project. Here's what the server said:\n" ++ message)
            , Cmd.none
            )

        ReportException exception ->
            ( model, Nothing, Opbeat.capture exception )
