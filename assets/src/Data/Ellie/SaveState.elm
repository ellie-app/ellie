module Data.Ellie.SaveState exposing (Error(..), SaveState(..), canSave, isWorking)

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)
import Data.Elm.Compiler.Error as CompilerError


type Error
    = TermsAcceptanceFailed ApiError
    | CompileFailed String
    | SignatureFailed ApiError
    | UploadFailed String


type SaveState
    = Ready
    | AwaitingTermsAcceptance
    | AcceptingTerms
    | Compiling
    | RequestingSignature (Result (List CompilerError.Error) String)
    | Uploading Revision
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
