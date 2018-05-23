module Pages.Editor.Types.Analysis
    exposing
        ( Analysis
        , Hint
        , completions
        , empty
        , hint
        , withCode
        , withModules
        , withToken
        )

import Char
import Dict exposing (Dict)
import Ellie.Constants as Constants
import Ellie.Ui.CodeEditor as CodeEditor exposing (Completions, Located(..), Token(..))
import Elm.Docs exposing (Binop, Module, Union)
import Elm.Version as Version exposing (Version)
import Parser exposing ((|.), (|=), Parser)
import Parser.LanguageKit
import Set exposing (Set)


type Analysis
    = Analysis
        { modules : List Module
        , imports : ImportIndex
        , tokens : TokenIndex
        , activeHint : Maybe Hint
        , moduleNesting : Dict String (Set String)
        , advancedToken : Located Token
        , completions : Completions
        }


empty : Analysis
empty =
    Analysis
        { modules = []
        , imports = Dict.empty
        , tokens = Dict.empty
        , activeHint = Nothing
        , moduleNesting = Dict.empty
        , advancedToken = CodeEditor.nowhere CodeEditor.Unknown
        , completions = CodeEditor.noCompletions
        }


withModules : List Module -> Analysis -> Analysis
withModules modules (Analysis stuff) =
    let
        preCompletions =
            { stuff
                | modules = modules
                , tokens = buildTokenIndex stuff.imports modules
                , moduleNesting = buildModuleNesting modules
            }
    in
    Analysis
        { preCompletions
            | completions =
                buildCompletions
                    stuff.advancedToken
                    (Analysis preCompletions)
        }


withCode : String -> Analysis -> Analysis
withCode elmCode (Analysis stuff) =
    let
        imports =
            elmCode
                |> parseImports
                |> buildImportIndex
    in
    Analysis
        { stuff
            | imports = imports
            , tokens = buildTokenIndex imports stuff.modules
        }


withToken : Located Token -> Analysis -> Analysis
withToken token ((Analysis stuff) as analysis) =
    Analysis
        { stuff
            | advancedToken = token
            , completions = buildCompletions token analysis
            , activeHint = findHint token analysis
        }


buildCompletions : Located Token -> Analysis -> Completions
buildCompletions (Located from to token) (Analysis analysis) =
    case token of
        Unknown ->
            CodeEditor.completions (Located from to [])

        Qualifier qualifier ->
            let
                moduleNames =
                    analysis.moduleNesting
                        |> Dict.get qualifier
                        |> Maybe.map Set.toList
                        |> Maybe.withDefault []

                values =
                    analysis.modules
                        |> List.filter (\mod -> mod.name == qualifier)
                        |> List.concatMap
                            (\mod ->
                                List.map .name mod.values
                                    ++ List.map .name mod.unions
                                    ++ List.concatMap (.tags >> List.map Tuple.first) mod.unions
                                    ++ List.map .name mod.aliases
                            )
            in
            CodeEditor.completions (Located from to (values ++ moduleNames))

        LowercaseVar text (Just qualifier) ->
            case List.head (List.filter (\mod -> mod.name == qualifier) analysis.modules) of
                Just mod ->
                    mod.values
                        |> List.filterMap
                            (\value ->
                                if String.startsWith text value.name && text /= value.name then
                                    Just value.name
                                else
                                    Nothing
                            )
                        |> Located from to
                        |> CodeEditor.completions

                Nothing ->
                    CodeEditor.noCompletions

        LowercaseVar text Nothing ->
            analysis.tokens
                |> Dict.keys
                |> List.filter (\s -> String.startsWith text s && s /= text)
                |> Located from to
                |> CodeEditor.completions

        _ ->
            CodeEditor.completions (Located from to [])


completions : Analysis -> Completions
completions (Analysis stuff) =
    stuff.completions


hint : Analysis -> Maybe Hint
hint (Analysis stuff) =
    stuff.activeHint


findHint : Located Token -> Analysis -> Maybe Hint
findHint (Located _ _ token) (Analysis stuff) =
    case token of
        Unknown ->
            Nothing

        Qualifier qualifier ->
            Nothing

        UppercaseVar name qualifier ->
            let
                fullName =
                    qualifier
                        |> Maybe.map (\q -> q ++ "." ++ name)
                        |> Maybe.withDefault name

                value =
                    Dict.get fullName stuff.tokens
                        |> Maybe.andThen List.head
            in
            case value of
                Just value ->
                    Just value

                Nothing ->
                    stuff.modules
                        |> List.filter (\mod -> mod.name == fullName)
                        |> List.head
                        |> Maybe.map
                            (\mod ->
                                { name = mod.name
                                , url = moduleUrl mod
                                }
                            )

        LowercaseVar name qualifier ->
            qualifier
                |> Maybe.map (\q -> Just (q ++ "." ++ name))
                |> Maybe.withDefault (Just name)
                |> Maybe.andThen (\n -> Dict.get n stuff.tokens)
                |> Maybe.andThen List.head

        Operator name ->
            stuff.tokens
                |> Dict.get name
                |> Maybe.andThen List.head


buildModuleNesting : List Module -> Dict String (Set String)
buildModuleNesting modules =
    List.foldl
        (\modul dict ->
            case String.split "." modul.name of
                [] ->
                    dict

                [ only ] ->
                    dict

                stuff ->
                    stuff
                        |> List.foldl
                            (\part ( maybeParent, output ) ->
                                case maybeParent of
                                    Just parent ->
                                        ( Just (parent ++ "." ++ part)
                                        , Dict.update parent
                                            (\maybeNestedValues ->
                                                case maybeNestedValues of
                                                    Just nestedValues ->
                                                        Just (Set.insert part nestedValues)

                                                    Nothing ->
                                                        Just (Set.singleton part)
                                            )
                                            output
                                        )

                                    Nothing ->
                                        ( Just part
                                        , output
                                        )
                            )
                            ( Nothing, dict )
                        |> Tuple.second
        )
        Dict.empty
        modules


type alias Hint =
    { name : String
    , url : String
    }


type alias TokenIndex =
    Dict String (List Hint)


buildTokenIndex : ImportIndex -> List Module -> TokenIndex
buildTokenIndex imports moduleList =
    let
        getMaybeHints moduleDocs =
            Maybe.map (filteredHints moduleDocs) (Dict.get moduleDocs.name imports)

        insert ( token, hint ) dict =
            Dict.update token (\value -> Just (hint :: Maybe.withDefault [] value)) dict
    in
    moduleList
        |> List.filterMap getMaybeHints
        |> List.concat
        |> List.foldl insert Dict.empty


filteredHints : Module -> Import -> List ( String, Hint )
filteredHints moduleData importData =
    let
        allNames =
            List.concat
                [ List.map .name moduleData.aliases
                , List.map .name moduleData.unions
                , List.map .name moduleData.values
                ]
    in
    List.concat
        [ List.concatMap (unionTagsToHints moduleData) moduleData.unions
        , List.concatMap (binopsToHints moduleData importData) moduleData.binops
        , List.concatMap (nameToHints moduleData importData) allNames
        ]


binopsToHints : Module -> Import -> Binop -> List ( String, Hint )
binopsToHints moduleData importData binop =
    if isExposed binop.name importData then
        let
            withParens =
                "(" ++ binop.name ++ ")"
        in
        [ ( binop.name, { name = moduleData.name ++ "." ++ withParens, url = urlTo moduleData withParens } ) ]
    else
        []


nameToHints : Module -> Import -> String -> List ( String, Hint )
nameToHints moduleDocs importData name =
    let
        fullName =
            moduleDocs.name ++ "." ++ name

        hint =
            { name = fullName, url = urlTo moduleDocs name }

        localName =
            Maybe.withDefault moduleDocs.name importData.alias
                ++ "."
                ++ name
    in
    if isExposed name importData then
        [ ( name, hint ), ( localName, hint ) ]
    else
        [ ( localName, hint ) ]


isExposed : String -> Import -> Bool
isExposed name importData =
    case importData.exposed of
        ExposedNone ->
            False

        ExposedSome set ->
            Set.member name set

        ExposedAll ->
            True


unionTagsToHints : Module -> Union -> List ( String, Hint )
unionTagsToHints moduleDocs union =
    let
        addHints ( tag, _ ) hints =
            let
                fullName =
                    moduleDocs.name ++ "." ++ tag

                hint =
                    Hint fullName (urlTo moduleDocs union.name)
            in
            ( tag, hint ) :: ( fullName, hint ) :: hints
    in
    List.foldl addHints [] union.tags


urlTo : Module -> String -> String
urlTo moduleData valueName =
    moduleUrl moduleData ++ "#" ++ valueName


moduleUrl : Module -> String
moduleUrl moduleData =
    "http://package.elm-lang.org/packages/"
        ++ moduleData.package.name.user
        ++ "/"
        ++ moduleData.package.name.project
        ++ "/"
        ++ Version.toString moduleData.package.version
        ++ "/"
        ++ dotToHyphen moduleData.name


dotToHyphen : String -> String
dotToHyphen string =
    String.map
        (\c ->
            if c == '.' then
                '-'
            else
                c
        )
        string



-- IMPORTS


type alias ImportIndex =
    Dict String Import


buildImportIndex : List Import -> ImportIndex
buildImportIndex imports =
    imports
        |> List.append defaultImports
        |> List.map (\i -> ( i.name, i ))
        |> Dict.fromList


defaultImports : List Import
defaultImports =
    [ Import "Basics" Nothing ExposedAll
    , Import "Debug" Nothing ExposedNone
    , Import "List" Nothing (ExposedSome (Set.fromList [ "List", "::" ]))
    , Import "Maybe" Nothing (ExposedSome (Set.singleton "Maybe"))
    , Import "Result" Nothing (ExposedSome (Set.singleton "Result"))
    , Import "Platform" Nothing (ExposedSome (Set.singleton "Program"))
    , Import "String" Nothing ExposedNone
    , Import "Platform.Cmd" (Just "Cmd") (ExposedSome (Set.fromList [ "Cmd", "!" ]))
    , Import "Platform.Sub" (Just "Sub") (ExposedSome (Set.singleton "Sub"))
    ]


type alias Import =
    { name : String
    , alias : Maybe String
    , exposed : Exposed
    }


type Exposed
    = ExposedAll
    | ExposedNone
    | ExposedSome (Set String)


parseImports : String -> List Import
parseImports code =
    code
        |> String.split "\n"
        |> List.filterMap (Parser.run importParser >> Result.toMaybe)


importParser : Parser Import
importParser =
    Parser.succeed (\n ( a, e ) -> Import n a e)
        |. Parser.keyword "import"
        |. spaces
        |= qualifiedVarParser
        |= detailsParser


detailsParser : Parser ( Maybe String, Exposed )
detailsParser =
    Parser.oneOf
        [ aliasParser
        , Parser.map (\e -> ( Nothing, e )) exposingParser
        , Parser.succeed ( Nothing, ExposedNone )
        ]


aliasParser : Parser ( Maybe String, Exposed )
aliasParser =
    Parser.delayedCommit asParser <|
        Parser.succeed (\s e -> ( Just s, e ))
            |= capVarParser
            |= exposingParser


asParser : Parser ()
asParser =
    Parser.succeed ()
        |. spaces
        |. Parser.keyword "as"
        |. spaces


exposingParser : Parser Exposed
exposingParser =
    Parser.oneOf
        [ Parser.delayedCommit exposingKwParser exposedParser
        , Parser.succeed ExposedNone
        ]


exposingKwParser : Parser ()
exposingKwParser =
    Parser.succeed ()
        |. spaces
        |. Parser.keyword "exposing"
        |. spaces


exposedParser : Parser Exposed
exposedParser =
    Parser.oneOf
        [ Parser.map (\_ -> ExposedAll) (Parser.symbol "(..)")
        , Parser.oneOf [ typeParser, lowerVarParser, infixParser ]
            |> Parser.LanguageKit.tuple spaces
            |> Parser.map (Set.fromList >> ExposedSome)
        , Parser.succeed ExposedNone
        ]


typeParser : Parser String
typeParser =
    Parser.succeed (\a -> a)
        |= capVarParser
        |. spaces
        |. constructorExportsParser


constructorExportsParser : Parser ()
constructorExportsParser =
    Parser.oneOf
        [ Parser.symbol "(..)"
        , Parser.map (\_ -> ()) <| Parser.LanguageKit.tuple spaces capVarParser
        , Parser.succeed ()
        ]


infixParser : Parser String
infixParser =
    Parser.succeed identity
        |. Parser.ignore (Parser.Exactly 1) ((==) '(')
        |= Parser.keep Parser.oneOrMore (\c -> not (isVarChar c) && c /= ')')
        |. Parser.ignore (Parser.Exactly 1) ((==) ')')


capVarParser : Parser String
capVarParser =
    Parser.LanguageKit.variable Char.isUpper isVarChar keywords


lowerVarParser : Parser String
lowerVarParser =
    Parser.LanguageKit.variable Char.isLower isVarChar keywords


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


qualifiedVarParser : Parser String
qualifiedVarParser =
    Parser.LanguageKit.variable Char.isUpper (\c -> isVarChar c || c == '.') keywords


keywords : Set String
keywords =
    Set.fromList [ "let", "in", "case", "of", "type", "import", "exposing", "as" ]


spaces : Parser ()
spaces =
    Parser.LanguageKit.whitespace
        { allowTabs = False
        , lineComment = Parser.LanguageKit.LineComment "--"
        , multiComment = Parser.LanguageKit.NestableComment "{-" "-}"
        }
