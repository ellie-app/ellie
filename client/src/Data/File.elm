module Data.File exposing (File, decoder, encoder, expect, fromStringParts, lastModified, name, toBody, toObjectUrl, toPart)

import Http exposing (Body, Expect, Part)
import Json.Decode as Decode exposing (Decoder, Value)
import Native.File
import Time exposing (Time)


type File
    = File


name : File -> String
name =
    Native.File.name


lastModified : File -> Time
lastModified =
    Native.File.lastModified


fromStringParts : String -> String -> List String -> File
fromStringParts =
    Native.File.fromStringParts


toObjectUrl : File -> String
toObjectUrl =
    Native.File.toObjectUrl


toBody : File -> Body
toBody =
    Native.File.fileBody


toPart : String -> File -> Part
toPart =
    Native.File.filePart


fromValue : Value -> Result String File
fromValue =
    Native.File.fromValue


decoder : Decoder File
decoder =
    Decode.andThen
        (\value ->
            case fromValue value of
                Ok file ->
                    Decode.succeed file

                Err msg ->
                    Decode.fail msg
        )
        Decode.value


encoder : File -> Value
encoder =
    Native.File.encoder


expect : String -> String -> Expect File
expect =
    Native.File.expect
