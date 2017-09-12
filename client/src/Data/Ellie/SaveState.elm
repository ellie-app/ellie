module Data.Ellie.SaveState exposing (Error(..), SaveState(..), canSave, isWorking)

import Data.Ellie.ApiError as ApiError exposing (ApiError)


type Error
    = TermsAcceptanceFailed ApiError
    | CompileFailed String
    | UploadFailed ApiError


type SaveState
    = Ready
    | AwaitingTermsAcceptance
    | AcceptingTerms
    | Compiling
    | Uploading
    | Failed Error


isWorking : SaveState -> Bool
isWorking saveState =
    case saveState of
        Ready ->
            False

        Failed _ ->
            False

        AwaitingTermsAcceptance ->
            False

        _ ->
            True


canSave : SaveState -> Bool
canSave saveState =
    case saveState of
        Ready ->
            True

        AwaitingTermsAcceptance ->
            True

        Failed _ ->
            True

        _ ->
            False
