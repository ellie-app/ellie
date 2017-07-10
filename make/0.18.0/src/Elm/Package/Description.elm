module Elm.Package.Description exposing (Description, read, decoder, encoder)

import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Extra.String as String
import Data.FilePath as FilePath exposing (FilePath, (</>))
import Elm.Compiler.Module as Module
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)
import Elm.Make.Constraint as Constraint exposing (Constraint)
import FileStorage as FileStorage


type alias Description =
    { name : Name
    , repo : String
    , version : Version
    , elmVersion : Constraint
    , summary : String
    , license : String
    , sourceDirs : List FilePath
    , exposed : List Module.Raw
    , natives : Bool
    , dependencies : List ( Name, Constraint )
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


nativesDecoder : Decoder Bool
nativesDecoder =
    Decode.oneOf
        [ Decode.bool
        , Decode.succeed False
        ]


exposedDecoder : Decoder Module.Raw
exposedDecoder =
    Decode.map
        (String.split ".")
        Decode.string


decodeKvp : List ( String, Constraint ) -> Decoder (List ( Name, Constraint ))
decodeKvp tuples =
    List.foldr
        (\( key, constraint ) memo ->
            Decode.andThen
                (\list ->
                    case Name.fromString key of
                        Just name ->
                            Decode.succeed (( name, constraint ) :: list)

                        Nothing ->
                            Decode.fail "Bad constraint in list"
                )
                memo
        )
        (Decode.succeed [])
        tuples


depsDecoder : Decoder (List ( Name, Constraint ))
depsDecoder =
    Decode.andThen decodeKvp (Decode.keyValuePairs Constraint.decoder)


nameFromRepoDecoder : Decoder Name
nameFromRepoDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case String.split "github.com/" value of
                    [ _, name ] ->
                        case Name.fromString (String.replace ".git" "" name) of
                            Just n ->
                                Decode.succeed n

                            _ ->
                                Decode.fail "Bad name"

                    _ ->
                        Decode.fail "Bad repo"
            )


decoder : Decoder Description
decoder =
    Decode.succeed Description
        |> andMap (Decode.field "repository" nameFromRepoDecoder)
        |> andMap (Decode.field "repository" Decode.string)
        |> andMap (Decode.field "version" Version.decoder)
        |> andMap (Decode.field "elm-version" Constraint.decoder)
        |> andMap (Decode.field "summary" Decode.string)
        |> andMap (Decode.field "license" Decode.string)
        |> andMap (Decode.field "source-directories" <| Decode.list Decode.string)
        |> andMap (Decode.field "exposed-modules" <| Decode.list exposedDecoder)
        |> andMap (Decode.succeed True)
        |> andMap (Decode.field "dependencies" depsDecoder)


encoder : Description -> Value
encoder description =
    Encode.object
        [ ( "repository", Encode.string description.repo )
        , ( "version", Encode.string <| Version.toString description.version )
        , ( "elm-version", Encode.string <| Constraint.toString description.elmVersion )
        , ( "summary", Encode.string description.summary )
        , ( "license", Encode.string description.license )
        , ( "source-directories", Encode.list <| List.map Encode.string description.sourceDirs )
        , ( "exposed-modules", Encode.list <| List.map (Module.nameToString >> Encode.string) description.exposed )
        , ( "native-modules", Encode.bool description.natives )
        , ( "dependencies"
          , Encode.object <| List.map (\( k, v ) -> ( Name.toString k, Encode.string <| Constraint.toString v )) description.dependencies
          )
        ]


read : (String -> e) -> FilePath -> Task e Description
read toError path =
    FileStorage.read path
        |> Task.andThen
            (\value ->
                case Decode.decodeValue decoder value of
                    Ok desc ->
                        Task.succeed desc

                    Err msg ->
                        Task.fail msg
            )
        |> Task.mapError toError
