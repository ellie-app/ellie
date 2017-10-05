module Pages.Editor.Save.Subscriptions exposing (..)

import Extra.Result as Result
import Pages.Editor.Save.Update exposing (Msg(..))
import Shared.Aws as Aws


awsToMsg : Aws.Inbound -> Msg
awsToMsg inbound =
    case inbound of
        Aws.UploadSucceeded { id } ->
            UploadSucceeded id

        Aws.UploadFailed { id, message } ->
            UploadFailed id message


subscriptions : Sub Msg
subscriptions =
    Sub.map
        (Result.fold awsToMsg ReportException)
        Aws.subscriptions
