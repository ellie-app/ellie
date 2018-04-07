module Pages.Editor.Types.Analysis
    exposing
        ( Analysis
        , empty
        , withCode
        , withModules
        )

import Elm.Docs exposing (Module)
import Regex exposing (Regex)
import Set exposing (Set)


type Analysis
    = Analysis
        { modules : List Module
        , imports : List Import
        }


empty : Analysis
empty =
    Analysis
        { modules = []
        , imports = []
        }


withModules : List Module -> Analysis -> Analysis
withModules modules (Analysis stuff) =
    Analysis { stuff | modules = modules }


withCode : String -> Analysis -> Analysis
withCode elmCode (Analysis stuff) =
    Analysis { stuff | imports = parseImports elmCode }


type alias Import =
    { name : String
    , alias : Maybe String
    , exposed : Exposed
    }


type Exposed
    = ExposedAll
    | ExposedNone
    | ExposedSome (Set String)


importRegex : Regex
importRegex =
    Regex.regex "(?:^|\\n)import\\s([\\w\\.]+)(?:\\sas\\s(\\w+))?(?:\\sexposing\\s*\\(((?:\\s*(?:\\w+|\\(.+\\))\\s*,)*)\\s*((?:\\.\\.|\\w+|\\(.+\\)))\\s*\\))?"


exposedAllRegex : Regex
exposedAllRegex =
    Regex.regex "^\\(\\s*\\.\\.\\)$"


parseImports : String -> List Import
parseImports code =
    code
        |> Regex.find Regex.All importRegex
        |> List.filterMap parseImportsHelp


parseImportsHelp : Regex.Match -> Maybe Import
parseImportsHelp match =
    case match.submatches of
        (Just nameString) :: aliasString :: exposedStringLeft :: exposedStringRight :: _ ->
            Just
                { name = nameString
                , alias = aliasString
                , exposed =
                    case Maybe.map2 (++) exposedStringLeft exposedStringRight of
                        Nothing ->
                            ExposedNone

                        Just "" ->
                            ExposedNone

                        Just varsString ->
                            if Regex.contains exposedAllRegex (String.trim varsString) then
                                ExposedAll
                            else
                                varsString
                                    |> String.trim
                                    |> String.dropLeft 1
                                    |> String.dropRight 1
                                    |> String.split ","
                                    |> List.map String.trim
                                    |> Set.fromList
                                    |> ExposedSome
                }

        _ ->
            Nothing
