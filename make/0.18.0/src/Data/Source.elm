module Data.Source exposing (Source, code, decoder, encoder, moduleName)

import Char
import Elm.Compiler.Module as Module
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Parser.LanguageKit as Parser
import Set exposing (Set)


type Source
    = Source String


type SourceTag
    = Normal
    | Effect
    | Port


encoder : Source -> Value
encoder (Source source) =
    Encode.string source


decoder : Decoder Source
decoder =
    Decode.map Source Decode.string


moduleName : Source -> Module.Raw
moduleName (Source source) =
    Parser.run moduleNameParser source
        |> Result.map (String.split ".")
        |> Result.withDefault [ "Main" ]


code : Source -> String
code (Source source) =
    source


moduleNameParser : Parser String
moduleNameParser =
    Parser.succeed identity
        |. sourceTag
        |. spaces
        |= qualifiedCapVar


spaces : Parser ()
spaces =
    Parser.whitespace
        { allowTabs = False
        , lineComment = Parser.NoLineComment
        , multiComment = Parser.NoMultiComment
        }


sourceTag : Parser SourceTag
sourceTag =
    Parser.oneOf
        [ Parser.succeed Normal
            |. Parser.keyword "module"
        , Parser.succeed Port
            |. Parser.keyword "port"
            |. spaces
            |. Parser.keyword "module"
        , Parser.succeed Effect
            |. Parser.keyword "effect"
            |. spaces
            |. Parser.keyword "module"
        ]


qualifiedCapVar : Parser String
qualifiedCapVar =
    Parser.andThen qualifiedCapVarEnd capVar


(|-) : Parser ignore -> Parser keep -> Parser keep
(|-) ignoreParser keepParser =
    Parser.map2 (\_ keep -> keep) ignoreParser keepParser


qualifiedCapVarEnd : String -> Parser String
qualifiedCapVarEnd current =
    let
        chompRest var =
            qualifiedCapVarEnd (current ++ "." ++ var)
    in
    Parser.oneOf
        [ Parser.symbol "."
            |- Parser.andThen chompRest capVar
        , Parser.succeed current
        ]


capVar : Parser String
capVar =
    Parser.variable
        Char.isUpper
        isVarChar
        keywords


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || (char == '_')


keywords : Set.Set String
keywords =
    Set.fromList [ "let", "in", "case", "of", "if", "then", "else" ]
