module Elm.Error exposing
    ( Error(..), BadModule, Problem
    , Chunk(..), Style, Color(..)
    , Region, Position
    , selection
    )

{-| When `elm make --report=json` fails, this module helps you turn the
resulting JSON into HTML.


# Compile Errors

@docs decoder, Error, BadModule, Problem


# Styled Text

@docs Chunk, Style, Color


# Code Regions

@docs Region, Position

-}

import Ellie.Api.Enum.ElmErrorColor as ElmErrorColor
import Ellie.Api.Object.ElmErrorBadModule as ElmErrorBadModule
import Ellie.Api.Object.ElmErrorChunk as ElmErrorChunk
import Ellie.Api.Object.ElmErrorGeneralProblem as ElmErrorGeneralProblem
import Ellie.Api.Object.ElmErrorModuleProblems as ElmErrorModuleProblems
import Ellie.Api.Object.ElmErrorPosition as ElmErrorPosition
import Ellie.Api.Object.ElmErrorProblem as ElmErrorProblem
import Ellie.Api.Object.ElmErrorRegion as ElmErrorRegion
import Ellie.Api.Object.ElmErrorStyle as ElmErrorStyle
import Ellie.Api.Union as ApiUnion
import Ellie.Api.Union.ElmError as ElmError
import Graphql.Field as Field
import Graphql.SelectionSet exposing (SelectionSet, with)


type Error
    = GeneralProblem { path : Maybe String, title : String, message : List Chunk }
    | ModuleProblems (List BadModule)


type alias BadModule =
    { path : String
    , name : String
    , problems : List Problem
    }


type alias Problem =
    { title : String
    , region : Region
    , message : List Chunk
    }


type Chunk
    = Unstyled String
    | Styled Style String


type alias Style =
    { bold : Bool
    , underline : Bool
    , color : Maybe Color
    }


type Color
    = Red
    | RED
    | Magenta
    | MAGENTA
    | Yellow
    | YELLOW
    | Green
    | GREEN
    | Cyan
    | CYAN
    | Blue
    | BLUE
    | White
    | WHITE
    | Black
    | BLACK


type alias Region =
    { start : Position
    , end : Position
    }


type alias Position =
    { line : Int
    , column : Int
    }


selection : SelectionSet Error ApiUnion.ElmError
selection =
    let
        badModuleSelection =
            ElmErrorBadModule.selection BadModule
                |> with ElmErrorBadModule.path
                |> with ElmErrorBadModule.name
                |> with (ElmErrorBadModule.problems problemSelection)

        problemSelection =
            ElmErrorProblem.selection Problem
                |> with ElmErrorProblem.title
                |> with (ElmErrorProblem.region regionSelection)
                |> with (ElmErrorProblem.message chunkSelection)

        regionSelection =
            ElmErrorRegion.selection Region
                |> with (ElmErrorRegion.start positionSelection)
                |> with (ElmErrorRegion.end positionSelection)

        positionSelection =
            ElmErrorPosition.selection Position
                |> with ElmErrorPosition.line
                |> with ElmErrorPosition.column

        chunkSelection =
            ElmErrorChunk.selection makeChunk
                |> with ElmErrorChunk.string
                |> with (ElmErrorChunk.style styleSelection)

        makeChunk string style =
            case style of
                Just x ->
                    Styled x string

                Nothing ->
                    Unstyled string

        styleSelection =
            ElmErrorStyle.selection Style
                |> with ElmErrorStyle.bold
                |> with ElmErrorStyle.underline
                |> with (Field.map (Maybe.map makeColor) ElmErrorStyle.color)

        makeColor color =
            case color of
                ElmErrorColor.Red ->
                    Red

                ElmErrorColor.VividRed ->
                    RED

                ElmErrorColor.Magenta ->
                    Magenta

                ElmErrorColor.VividMagenta ->
                    MAGENTA

                ElmErrorColor.Yellow ->
                    Yellow

                ElmErrorColor.VividYellow ->
                    YELLOW

                ElmErrorColor.Green ->
                    Green

                ElmErrorColor.VividGreen ->
                    GREEN

                ElmErrorColor.Cyan ->
                    Cyan

                ElmErrorColor.VividCyan ->
                    CYAN

                ElmErrorColor.Blue ->
                    Blue

                ElmErrorColor.VividBlue ->
                    BLUE

                ElmErrorColor.White ->
                    White

                ElmErrorColor.VividWhite ->
                    WHITE

                ElmErrorColor.Black ->
                    Black

                ElmErrorColor.VividBlack ->
                    BLACK
    in
    ElmError.selection (Maybe.withDefault (ModuleProblems []))
        [ ElmErrorGeneralProblem.selection (\path title message -> GeneralProblem { path = path, title = title, message = message })
            |> with ElmErrorGeneralProblem.path
            |> with ElmErrorGeneralProblem.title
            |> with (ElmErrorGeneralProblem.message chunkSelection)
            |> ElmError.onElmErrorGeneralProblem
        , ElmErrorModuleProblems.selection ModuleProblems
            |> with (ElmErrorModuleProblems.errors badModuleSelection)
            |> ElmError.onElmErrorModuleProblems
        ]
