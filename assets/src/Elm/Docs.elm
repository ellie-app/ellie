module Elm.Docs
    exposing
        ( Alias
        , Associativity(..)
        , Binop
        , Block(..)
        , Module
        , Union
        , Value
        , selection
        , toBlocks
        )

{-| When packages are published to [package.elm-lang.org][pkg], documentation
is generated for all of the exposed modules (and all of the exposed values).
These docs are formatted as JSON for easy consumption by anyone.

[pkg]: http://package.elm-lang.org/

This module helps you decode the JSON docs into nice Elm values! It is
currently used by [package.elm-lang.org][pkg] to help turn JSON into nice
web pages!


# Decode Docs

@docs decoder


# Work with Docs

@docs Module, Alias, Union, Value, Binop, Associativity


# Split Docs into Blocks

@docs toBlocks, Block

-}

import Ellie.Api.Enum.ElmDocsAssociativity as ElmDocsAssociativity
import Ellie.Api.Object exposing (ElmDocsModule)
import Ellie.Api.Object.ElmDocsAlias as ElmDocsAlias
import Ellie.Api.Object.ElmDocsBinop as ElmDocsBinop
import Ellie.Api.Object.ElmDocsModule as ElmDocsModule
import Ellie.Api.Object.ElmDocsTag as ElmDocsTag
import Ellie.Api.Object.ElmDocsUnion as ElmDocsUnion
import Ellie.Api.Object.ElmDocsValue as ElmDocsValue
import Ellie.Api.Scalar as Scalar
import Elm.Package as Package exposing (Package)
import Graphqelm.Field as Field
import Graphqelm.SelectionSet exposing (SelectionSet, hardcoded, with)


-- DOCUMENTATION


{-| All the documentation for a particular module.

  - `name` is the module name
  - `comment` is the module comment

The actual exposed stuff is broken into categories.

-}
type alias Module =
    { name : String
    , comment : String
    , unions : List Union
    , aliases : List Alias
    , values : List Value
    , binops : List Binop
    , package : Package
    }


{-| Documentation for a type alias. For example, if you had the source code:

    type alias Pair a =
        ( a, a )

When it became an `Alias` it would be like this:

    { name = "Pair"
    , comment = " pair of values "
    , args = ["a"]
    , tipe = Tuple [ Var "a", Var "a" ]
    }

-}
type alias Alias =
    { name : String
    , comment : String
    , args : List String
    , tipe : String
    }


{-| Documentation for a union type. For example, if you had the source code:

    type Maybe a
        = Nothing
        | Just a

When it became a `Union` it would be like this:

    { name = "Maybe"
    , comment = " maybe "
    , args = ["a"]
    , tipe =
        [ ("Nothing", [])
        , ("Just", [Var "a"])
        ]
    }

-}
type alias Union =
    { name : String
    , comment : String
    , args : List String
    , tags : List ( String, List String )
    }


{-| Documentation for values and functions. For example, if you had the source
code:

    identity : a -> a
    identity value =
        value

The `Value` would look like this:

    { name = "identity"
    , comment = " do not do anything "
    , tipe = Lambda (Var "a") (Var "a")
    }

-}
type alias Value =
    { name : String
    , comment : String
    , tipe : String
    }


{-| Documentation for binary operators. The content for `(+)` might look
something like this:

    { name = "+"
    , comment = "Add numbers"
    , tipe = Lambda (Var "number") (Lambda (Var "number") (Var "number"))
    , associativity = Left
    , precedence = 6
    }8

-}
type alias Binop =
    { name : String
    , comment : String
    , tipe : String
    , associativity : Associativity
    , precedence : Int
    }


{-| The [associativity] of an infix operator. This determines how we add
parentheses around everything. Here are some examples:

    1 + 2 + 3 + 4

We have to do the operations in _some_ order, so which of these interpretations
should we choose?

    ((1 + 2) + 3) + 4   -- left-associative
    1 + (2 + (3 + 4))   -- right-associative

This is really important for operators like `(|>)`!

Some operators are non-associative though, meaning we do not try to add
missing parentheses. `(==)` is a nice example. `1 == 2 == 3` just is not
allowed!

[associativity]: https://en.wikipedia.org/wiki/Operator_associativity

-}
type Associativity
    = Left
    | None
    | Right



-- SELECTION


selection : SelectionSet (Package -> Module) ElmDocsModule
selection =
    let
        selection_ =
            ElmDocsModule.selection Module
                |> with ElmDocsModule.name
                |> with ElmDocsModule.comment
                |> with (ElmDocsModule.unions unionSelection)
                |> with (ElmDocsModule.aliases aliasSelection)
                |> with (ElmDocsModule.values valueSelection)
                |> with (ElmDocsModule.binops binopSelection)

        unionSelection =
            ElmDocsUnion.selection Union
                |> with ElmDocsUnion.name
                |> with ElmDocsUnion.comment
                |> with ElmDocsUnion.args
                |> with (ElmDocsUnion.tags tagSelection)

        aliasSelection =
            ElmDocsAlias.selection Alias
                |> with ElmDocsAlias.name
                |> with ElmDocsAlias.comment
                |> with ElmDocsAlias.args
                |> with (Field.map (\(Scalar.ElmDocsType t) -> t) ElmDocsAlias.type_)

        valueSelection =
            ElmDocsValue.selection Value
                |> with ElmDocsValue.name
                |> with ElmDocsValue.comment
                |> with (Field.map (\(Scalar.ElmDocsType t) -> t) ElmDocsValue.type_)

        binopSelection =
            ElmDocsBinop.selection Binop
                |> with ElmDocsBinop.name
                |> with ElmDocsBinop.comment
                |> with (Field.map (\(Scalar.ElmDocsType t) -> t) ElmDocsBinop.type_)
                |> with (Field.map makeAssociativity ElmDocsBinop.associativity)
                |> with ElmDocsBinop.precedence

        tagSelection =
            ElmDocsTag.selection (,)
                |> with ElmDocsTag.name
                |> with (Field.map (List.map (\(Scalar.ElmDocsType t) -> t)) ElmDocsTag.args)

        makeAssociativity a =
            case a of
                ElmDocsAssociativity.Left ->
                    Left

                ElmDocsAssociativity.Right ->
                    Right

                ElmDocsAssociativity.None ->
                    None
    in
    selection_



-- TO BLOCKS


{-| This type represents a `Block` of documentation to show to the user.
After getting a `List Block` from `toBlocks`, everything is in the right order
and you can focus on turning the blocks into HTML exactly how you want.

**Note:** This should never produce an `UnknownBlock` but I figured it
would be better to let the block visualizer decide what to do in that case.

-}
type Block
    = MarkdownBlock String
    | UnionBlock Union
    | AliasBlock Alias
    | ValueBlock Value
    | BinopBlock Binop
    | UnknownBlock String


{-| The module comment describes exactly how the generated docs should look.
It is a mix of markdown and `@docs` declarations that specify when other
documentation should appear. Matching all this information up is somewhat
tricky though.

So calling `toBlocks` on a `Module` gives you a `List Block` with all the
information necessary to visualize the docs as intended.

-}
toBlocks : Module -> List Block
toBlocks docs =
    case String.split "\n@docs " docs.comment of
        [] ->
            []

        firstMarkdown :: docsChunks ->
            MarkdownBlock firstMarkdown
                :: List.concatMap (chunkToBlocks docs) docsChunks


chunkToBlocks : Module -> String -> List Block
chunkToBlocks docs chunk =
    partsToBlocks docs (String.split "," chunk)


partsToBlocks : Module -> List String -> List Block
partsToBlocks docs parts =
    case parts of
        [] ->
            []

        part :: otherParts ->
            case String.words (String.trim part) of
                [] ->
                    [ MarkdownBlock <| String.join "," parts ]

                [ name ] ->
                    nameToBlock docs name
                        :: partsToBlocks docs otherParts

                name :: _ ->
                    [ nameToBlock docs name
                    , MarkdownBlock <|
                        String.join "," <|
                            String.dropLeft (String.length name) (String.trimLeft part)
                                :: otherParts
                    ]


nameToBlock : Module -> String -> Block
nameToBlock docs docsName =
    let
        name =
            if String.startsWith "(" docsName then
                String.dropLeft 1 (String.dropRight 1 docsName)
            else
                docsName
    in
    find ValueBlock name docs.values <|
        find BinopBlock name docs.binops <|
            find UnionBlock name docs.unions <|
                find AliasBlock name docs.aliases <|
                    UnknownBlock name


type alias Info r =
    { r | name : String }


find : (Info r -> Block) -> String -> List (Info r) -> Block -> Block
find toBlock name entries fallback =
    case entries of
        [] ->
            fallback

        entry :: rest ->
            if entry.name == name then
                toBlock entry
            else
                find toBlock name rest fallback
