module Data.Url.Parser.Query
    exposing
        ( Parser
        , Problem(..)
        , custom
        , enum
        , int
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , string
        )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

      https://example.com:8042/over/there?name=ferret#nose
      \___/   \______________/\_________/ \_________/ \__/
        |            |            |            |        |
      scheme     authority       path        query   fragment

This module is for parsing the `query` part.

In this library, a valid query looks like `?search=hats&page=2` where each
query parameter has the format `key=value` and is separated from the next
parameter by the `&` character.


# Parse Query Parameters

@docs Parser, string, int, enum, Problem, custom


# Mapping

@docs map, map2, map3, map4, map5, map6, map7, map8

-}

import Data.Url.Parser.Internal as Q
import Dict


-- PARSERS


{-| Parse a query like `?search=hat&page=2` into nice Elm data.
-}
type alias Parser a =
    Q.QueryParser a



-- PRIMITIVES


{-| Handle `String` parameters.

    search : Parser (Result Problem String)
    search =
        string "search"


    -- ?search=cats             == Ok "cats"
    -- ?search=42               == Ok "42"
    -- ?branch=left             == Err NotFound
    -- ?search=cats&search=dogs == Err (TooMany ["cats","dogs"])

Check out [`custom`](#custom) if you need to handle multiple `search`
parameters for some reason.

-}
string : String -> Parser (Result Problem String)
string key =
    custom key <|
        \stringList ->
            case stringList of
                [] ->
                    Err NotFound

                [ str ] ->
                    Ok str

                _ ->
                    Err (TooMany stringList)


{-| Handle `Int` parameters. Maybe you want to show paginated search results:

    page : Parser (Result Problem Int)
    page =
        int "page"


    -- ?page=2        == Ok 2
    -- ?page=17       == Ok 17
    -- ?page=two      == Err (Invalid "two")
    -- ?sort=date     == Err NotFound
    -- ?page=2&page=3 == Err (TooMany ["2","3"])

Check out [`custom`](#custom) if you need to handle multiple `page` parameters
or something like that.

-}
int : String -> Parser (Result Problem Int)
int key =
    custom key <|
        \stringList ->
            case stringList of
                [] ->
                    Err NotFound

                [ str ] ->
                    case String.toInt str of
                        Err _ ->
                            Err (Invalid str)

                        Ok n ->
                            Ok n

                _ ->
                    Err (TooMany stringList)


{-| Handle enumerated parameters. Maybe you want a true-or-false parameter:

    import Dict

    debug : Parser (Result Problem Bool)
    debug =
        enum "debug" (Dict.fromList [ ( "true", True ), ( "false", False ) ])


    -- ?debug=true   == Ok True
    -- ?debug=false  == Ok False
    -- ?debug=1      == Err (Invalid "1")
    -- ?debug=0      == Err (Invalid "0")

You could add `0` and `1` to the dictionary if you want to handle those as
well. You can also use [`map`](#map) to say `map (Result.withDefault False) debug`
to get a parser of type `Parser Bool` that swallows any errors and defaults to
`False`.

**Note:** Parameters like `?debug` with no `=` are not supported by this library.

-}
enum : String -> Dict.Dict String a -> Parser (Result Problem a)
enum key dict =
    custom key <|
        \stringList ->
            case stringList of
                [] ->
                    Err NotFound

                [ str ] ->
                    case Dict.get str dict of
                        Nothing ->
                            Err (Invalid str)

                        Just value ->
                            Ok value

                _ ->
                    Err (TooMany stringList)



-- PROBLEMS


{-| The [`string`](#string), [`int`](#int), and [`enum`](#enum) parsers may
fail for a few reasons.

  - `NotFound` means there was no parameter with that name.
  - `Invalid` means the parameter was found, but the value was not valid.
  - `TooMany` means Q found more than one paramater with that name!

If you actually _want_ more than one parameter with the same name, check out
the [`custom`](#custom) function below!

And if you want to ignore your problems and hope everything turns out okay, you
can write code like this:

    map (Result.withDefault 1) (int "page")

-}
type Problem
    = NotFound
    | Invalid String
    | TooMany (List String)



-- CUSTOM PARSERS


{-| Create a custom query parser. The [`string`](#string), [`int`](#int), and
[`enum`](#enum) parsers are defined using this function. It can help you handle
anything though!

Say you are unlucky enough to need to handle `?post=2&post=7` to show a couple
posts on screen at once. You could say:

    posts : Parser (Maybe (List Int))
    posts =
        custom "post" (List.maybeMap String.toInt)

-}
custom : String -> (List String -> a) -> Parser a
custom key func =
    Q.Parser <|
        \dict ->
            func (Maybe.withDefault [] (Dict.get key dict))



-- MAPPING


{-| Transform a parser in some way. Maybe you want your `page` query parser to
default to `1` if there is any problem?

    page : Parser Int
    page =
        map (Result.withDefault 1) (int "page")

-}
map : (a -> b) -> Parser a -> Parser b
map func (Q.Parser a) =
    Q.Parser <| \dict -> func (a dict)


{-| Combine two parsers. A query like `?search=hats&page=2` could be parsed
with something like this:

    type alias Query =
        { search : Result Problem String
        , page : Result Problem Int
        }

    query : Parser Query
    query =
        map2 Query (string "search") (int "page")

-}
map2 : (a -> b -> result) -> Parser a -> Parser b -> Parser result
map2 func (Q.Parser a) (Q.Parser b) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict)


{-| Combine three parsers. A query like `?search=hats&page=2&sort=ascending`
could be parsed with something like this:

    import Dict

    type alias Query =
        { search : Result Problem String
        , page : Result Problem Int
        , sort : Result Problem Order
        }

    type Order
        = Ascending
        | Descending

    query : Parser Query
    query =
        map3 Query (string "search") (int "page") (enum "sort" order)

    order : Dict.Dict String Order
    order =
        Dict.fromList
            [ ( "ascending", Ascending )
            , ( "descending", Descending )
            ]

-}
map3 : (a -> b -> c -> result) -> Parser a -> Parser b -> Parser c -> Parser result
map3 func (Q.Parser a) (Q.Parser b) (Q.Parser c) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict)


{-| -}
map4 : (a -> b -> c -> d -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser result
map4 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict)


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser result
map5 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict)


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser result
map6 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict)


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser result
map7 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) (Q.Parser g) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict)


{-| If you need higher than eight, you can define a function like this:

    apply : Parser a -> Parser (a -> b) -> Parser b
    apply argParser funcParser =
        map2 (<|) funcParser argParser

And then you can chain it to do as many of these as you would like:

    map func (string "search")
      |> apply (int "page")
      |> apply (int "per-page")

-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser result
map8 func (Q.Parser a) (Q.Parser b) (Q.Parser c) (Q.Parser d) (Q.Parser e) (Q.Parser f) (Q.Parser g) (Q.Parser h) =
    Q.Parser <|
        \dict ->
            func (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict) (h dict)
