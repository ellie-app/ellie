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
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


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
            SelectionSet.succeed BadModule
                |> SelectionSet.with ElmErrorBadModule.path
                |> SelectionSet.with ElmErrorBadModule.name
                |> SelectionSet.with (ElmErrorBadModule.problems problemSelection)

        problemSelection =
            SelectionSet.succeed Problem
                |> SelectionSet.with ElmErrorProblem.title
                |> SelectionSet.with (ElmErrorProblem.region regionSelection)
                |> SelectionSet.with (ElmErrorProblem.message chunkSelection)

        regionSelection =
            SelectionSet.succeed Region
                |> SelectionSet.with (ElmErrorRegion.start positionSelection)
                |> SelectionSet.with (ElmErrorRegion.end positionSelection)

        positionSelection =
            SelectionSet.succeed Position
                |> SelectionSet.with ElmErrorPosition.line
                |> SelectionSet.with ElmErrorPosition.column

        chunkSelection =
            SelectionSet.succeed makeChunk
                |> SelectionSet.with ElmErrorChunk.string
                |> SelectionSet.with (ElmErrorChunk.style styleSelection)

        makeChunk string maybeStyle =
            case maybeStyle of
                Just style ->
                    Styled style string

                Nothing ->
                    Unstyled string

        styleSelection =
            SelectionSet.succeed Style
                |> SelectionSet.with ElmErrorStyle.bold
                |> SelectionSet.with ElmErrorStyle.underline
                |> SelectionSet.with (SelectionSet.map (Maybe.map makeColor) ElmErrorStyle.color)

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
    ElmError.fragments
        { onElmErrorGeneralProblem =
            SelectionSet.succeed (\path title message -> GeneralProblem { path = path, title = title, message = message })
                |> SelectionSet.with ElmErrorGeneralProblem.path
                |> SelectionSet.with ElmErrorGeneralProblem.title
                |> SelectionSet.with (ElmErrorGeneralProblem.message chunkSelection)
        , onElmErrorModuleProblems =
            SelectionSet.succeed ModuleProblems
                |> SelectionSet.with (ElmErrorModuleProblems.errors badModuleSelection)
        }
