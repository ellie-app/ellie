module Elm.Docs
    exposing
        ( Alias
        , Associativity(..)
        , Binop
        , Block(..)
        , Module
        , Union
        , Value
        , decoder
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

import Elm.Type as Type exposing (Type)
import Json.Decode exposing (..)


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
    , tipe : Type
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
    , tags : List ( String, List Type )
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
    , tipe : Type
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
    , tipe : Type
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



-- DECODE


{-| Decode the JSON documentation produced by `elm-make` for an individual
module. The documentation for a whole package is an array of module docs,
so you may need to say `(Decode.list Docs.decoder)` depending on what you
want to do.
-}
decoder : Decoder Module
decoder =
    map6 Module
        (field "name" string)
        (field "comment" string)
        (field "unions" (list unionDecoder))
        (field "aliases" (list aliasDecoder))
        (field "values" (list valueDecoder))
        (field "binops" (list binopDecoder))


aliasDecoder : Decoder Alias
aliasDecoder =
    map4 Alias
        (field "name" string)
        (field "comment" string)
        (field "args" (list string))
        (field "type" Type.decoder)


unionDecoder : Decoder Union
unionDecoder =
    map4 Union
        (field "name" string)
        (field "comment" string)
        (field "args" (list string))
        (field "cases" (list tagDecoder))


tagDecoder : Decoder ( String, List Type )
tagDecoder =
    map2 (\a b -> ( a, b ))
        (index 0 string)
        (index 1 (list Type.decoder))


valueDecoder : Decoder Value
valueDecoder =
    map3 Value
        (field "name" string)
        (field "comment" string)
        (field "type" Type.decoder)


binopDecoder : Decoder Binop
binopDecoder =
    map5 Binop
        (field "name" string)
        (field "comment" string)
        (field "type" Type.decoder)
        (field "associativity" assocDecoder)
        (field "precedence" int)


assocDecoder : Decoder Associativity
assocDecoder =
    andThen toAssoc string


toAssoc : String -> Decoder Associativity
toAssoc str =
    case str of
        "left" ->
            succeed Left

        "non" ->
            succeed None

        "right" ->
            succeed Right

        _ ->
            fail "expecting one of the following values: left, non, right"



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
