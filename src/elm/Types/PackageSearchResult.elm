module Types.PackageSearchResult
    exposing
        ( PackageSearchResult
        , encode
        , decode
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import List.Nonempty as Nonempty exposing (Nonempty)
import Types.Version as Version exposing (Version)


decodeNonempty : Decoder a -> Decoder (Nonempty a)
decodeNonempty elementDecoder =
    elementDecoder
        |> Decode.list
        |> Decode.andThen
            (\values ->
                case Nonempty.fromList (Debug.log "v" values) of
                    Just nonempty ->
                        Decode.succeed nonempty

                    Nothing ->
                        Decode.fail "List was empty"
            )


type alias PackageSearchResult =
    { username : String
    , name : String
    , versions : Nonempty Version
    }


encode : PackageSearchResult -> Value
encode package =
    Encode.object
        [ ( "username", Encode.string package.username )
        , ( "name", Encode.string package.name )
        , ( "versions", Encode.list <| Nonempty.toList <| Nonempty.map Version.encode package.versions )
        ]


decode : Decoder PackageSearchResult
decode =
    Decode.decode PackageSearchResult
        |> Decode.required "username" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "versions" (decodeNonempty Version.decode)
