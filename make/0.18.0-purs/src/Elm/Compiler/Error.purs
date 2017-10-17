module Elm.Compiler.Error where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Generic (class Generic)
import Data.Maybe (Maybe)

-- LOCATION

newtype Location =
  Location
    { line :: Int
    , column :: Int
    }

derive instance genericLocation :: Generic Location
instance decodeJsonLocation :: DecodeJson Location where decodeJson = gDecodeJson
instance encodeJsonLocation :: EncodeJson Location where encodeJson = gEncodeJson

-- REGION

newtype Region =
  Region
    { start :: Location
    , end :: Location
    }

derive instance genericRegion :: Generic Region
instance decodeJsonRegion :: DecodeJson Region where decodeJson = gDecodeJson
instance encodeJsonRegion :: EncodeJson Region where encodeJson = gEncodeJson

-- ERROR

newtype Error =
  Error
    { tag :: String
    , overview :: String
    , details :: String
    , subregion :: Maybe Region
    , region :: Region
    , level :: String
    }

derive instance genericError :: Generic Error
instance decodeJsonError :: DecodeJson Error where decodeJson = gDecodeJson
instance encodeJsonError :: EncodeJson Error where encodeJson = gEncodeJson
