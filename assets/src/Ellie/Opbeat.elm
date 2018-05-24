port module Ellie.Opbeat exposing (Exception, capture)


type alias Exception =
    { tag : String
    , message : String
    , moduleName : String
    , line : Int
    , extraData : List ( String, String )
    }


port ellieOpbeatOut : Exception -> Cmd msg


capture : Exception -> Cmd msg
capture exception =
    ellieOpbeatOut exception
