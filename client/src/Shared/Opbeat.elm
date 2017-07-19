port module Shared.Opbeat exposing (Exception, capture)


type alias Exception =
    { tag : String
    , message : String
    , moduleName : String
    , line : Int
    , extraData : List ( String, String )
    }


port opbeatCaptureOut : Exception -> Cmd msg


capture : Exception -> Cmd msg
capture exception =
    opbeatCaptureOut exception
