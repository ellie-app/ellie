module Data.Url.Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , Protocol
        , Url
        , custom
        , fragment
        , fromUrl
        , int
        , map
        , oneOf
        , parse
        , query
        , s
        , string
        , toUrl
        , top
        )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

      https://example.com:8042/over/there?name=ferret#nose
      \___/   \______________/\_________/ \_________/ \__/
        |            |            |            |        |
      scheme     authority       path        query   fragment

This module is for parsing the `path` part.


# Primitives

@docs Parser, string, int, s


# Path

@docs (</>), map, oneOf, top, custom


# Query

@docs (<?>), query


# Fragment

@docs fragment


# Run Parsers

@docs parse, Url, Protocol, toUrl, fromUrl

-}

import Data.Url.Parser.Internal as Q
import Data.Url.Parser.Query as Query
import Dict exposing (Dict)
import Http


-- INFIX TABLE


(</>) : Parser a b -> Parser b c -> Parser a c
(</>) =
    slash
infixr 7 </>


(<?>) : Parser a (query -> b) -> Query.Parser query -> Parser a b
(<?>) =
    questionMark
infixl 8 <?>



-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
-}
type Parser a b
    = Parser (State a -> List (State b))


type alias State value =
    { visited : List String
    , unvisited : List String
    , params : Dict String (List String)
    , frag : Maybe String
    , value : value
    }



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.

    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
    -- /        ==>  Nothing

-}
string : Parser (String -> a) a
string =
    custom "STRING" Just


{-| Parse a segment of the path as an `Int`.

    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
    -- /        ==>  Nothing

-}
int : Parser (Int -> a) a
int =
    custom "NUMBER" (String.toInt >> Result.toMaybe)


{-| Parse a segment of the path if it matches a given string. It is almost
always used with [`</>`](#</>) or [`oneOf`](#oneOf). For example:

    blog : Parser (String -> a) a
    blog =
        s "blog" </> int


    -- /blog/42  ==>  Just 42
    -- /tree/42  ==>  Nothing

The path segment must be an exact match!

-}
s : String -> Parser a a
s str =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    if next == str then
                        [ State (next :: visited) rest params frag value ]
                    else
                        []


{-| Create a custom path segment parser. Here is how it is used to define the
`int` parser:

    int : Parser (Int -> a) a
    int =
        custom "NUMBER" String.toInt

You can use it to define something like “only CSS files” like this:

    css : Parser (String -> a) a
    css =
        custom "CSS_FILE" <|
            \segment ->
                if String.endsWith ".css" segment then
                    Just segment
                else
                    Nothing

-}
custom : String -> (String -> Maybe a) -> Parser (a -> b) b
custom tipe stringToSomething =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    case stringToSomething next of
                        Just nextValue ->
                            [ State (next :: visited) rest params frag (value nextValue) ]

                        Nothing ->
                            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.

    blog : Parser (Int -> a) a
    blog =
        s "blog" </> int


    -- /blog/35/  ==>  Just 35
    -- /blog/42   ==>  Just 42
    -- /blog/     ==>  Nothing
    -- /42/       ==>  Nothing

    search : Parser (String -> a) a
    search =
        s "search" </> string


    -- /search/cats/  ==>  Just "cats"
    -- /search/frog   ==>  Just "frog"
    -- /search/       ==>  Nothing
    -- /cats/         ==>  Nothing

-}
slash : Parser a b -> Parser b c -> Parser a c
slash (Parser parseBefore) (Parser parseAfter) =
    Parser <|
        \state ->
            List.concatMap parseAfter (parseBefore state)


{-| Transform a path parser.

    type alias Comment =
        { user : String, id : Int }

    userAndId : Parser (String -> Int -> a) a
    userAndId =
        s "user" </> string </> s "comments" </> int

    comment : Parser (Comment -> a) a
    comment =
        map Comment userAndId


    -- /user/bob/comments/42  ==>  Just { user = "bob", id = 42 }
    -- /user/tom/comments/35  ==>  Just { user = "tom", id = 35 }
    -- /user/sam/             ==>  Nothing

-}
map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parseArg) =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            List.map (mapState value) <|
                parseArg <|
                    State visited unvisited params frag subValue


mapState : (a -> b) -> State a -> State b
mapState func { visited, unvisited, params, frag, value } =
    State visited unvisited params frag (func value)


{-| Try a bunch of different path parsers.

    type Route
        = Search String
        | Blog Int
        | User String
        | Comment String Int

    route : Parser (Route -> a) a
    route =
        oneOf
            [ map Search (s "search" </> string)
            , map Blog (s "blog" </> int)
            , map User (s "user" </> string)
            , map Comment (s "user" </> string </> s "comments" </> int)
            ]


    -- /search/cats           ==>  Just (Search "cats")
    -- /search/"              ==>  Nothing
    -- /blog/42               ==>  Just (Blog 42)
    -- /blog/cats             ==>  Nothing
    -- /user/sam/             ==>  Just (User "sam")
    -- /user/bob/comments/42  ==>  Just (Comment "bob" 42)
    -- /user/tom/comments/35  ==>  Just (Comment "tom" 35)
    -- /user/                 ==>  Nothing

If there are multiple parsers that could succeed, the first one wins.

-}
oneOf : List (Parser a b) -> Parser a b
oneOf parsers =
    Parser <|
        \state ->
            List.concatMap (\(Parser parser) -> parser state) parsers


{-| A parser that does not consume any path segments.

    type Route
        = Overview
        | Post Int

    blog : Parser (BlogRoute -> a) a
    blog =
        s "blog"
            </> oneOf
                    [ map Overview top
                    , map Post (s "post" </> int)
                    ]


    -- /blog/         ==>  Just Overview
    -- /blog/post/42  ==>  Just (Post 42)

-}
top : Parser a a
top =
    Parser <| \state -> [ state ]



-- QUERY


{-| The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function helps you use those
with normal parsers. For example, maybe you want to add a search feature to
your blog website:

    import Url.Parser.Query as Query

    type Route
        = Overview (Result Query.Problem String)
        | Post Int

    blog : Parser (Route -> a) a
    blog =
        oneOf
            [ map Overview (s "blog" <?> Query.string "q")
            , map Post (s "blog" </> int)
            ]


    -- /blog/           ==>  Just (Overview (Err Query.NotFound))
    -- /blog/?q=cats    ==>  Just (Overview (Ok "cats"))
    -- /blog/cats       ==>  Nothing
    -- /blog/42         ==>  Just (Post 42)
    -- /blog/42?q=cats  ==>  Just (Post 42)
    -- /blog/42/cats    ==>  Nothing

-}
questionMark : Parser a (query -> b) -> Query.Parser query -> Parser a b
questionMark parser queryParser =
    slash parser (query queryParser)


{-| The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function is a helper to convert
those into normal parsers.

    import Url.Parser.Query as Query

    -- the following expressions are both the same!
    --
    -- s "blog" <?> Query.string "search"
    -- s "blog" </> query (Query.string "search")

This may be handy if you need query parameters but are not parsing any path
segments.

-}
query : Query.Parser query -> Parser (query -> a) a
query (Q.Parser queryParser) =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            [ State visited unvisited params frag (value (queryParser params))
            ]



-- FRAGMENT


{-| Create a parser for the URL fragment, the stuff after the `#`. This can
be handy for handling links to DOM elements within a page. Pages like this one!

    type alias Docs =
        { name : String
        , value : Maybe String
        }

    docs : Parser (Docs -> a) a
    docs =
        map Docs (string </> fragment identity)


    -- /List/map   ==>  Nothing
    -- /List/#map  ==>  Just (Docs "List" (Just "map"))
    -- /List#map   ==>  Just (Docs "List" (Just "map"))
    -- /List#      ==>  Just (Docs "List" (Just ""))
    -- /List       ==>  Just (Docs "List" Nothing)
    -- /           ==>  Nothing

-}
fragment : (Maybe String -> fragment) -> Parser (fragment -> a) a
fragment toFrag =
    Parser <|
        \{ visited, unvisited, params, frag, value } ->
            [ State visited unvisited params frag (value (toFrag frag))
            ]



-- PARSE


{-| Actually run a parser! You provide some [`Url`](#Url) that
represent a valid URL. From there `parse` runs your parser on the path, query
parameters, and fragment!

    import Url.Parser as Parser exposing (Parser, int, map, oneOf, s, top)

    type Route
        = Home
        | Blog Int
        | NotFound

    route : Parser (Route -> a) a
    route =
        oneOf
            [ map Home top
            , map Blog (s "blog" </> int)
            ]

    toRoute : String -> Route
    toRoute url =
        case Parser.toUrl url of
            Nothing ->
                NotFound

            Just segments ->
                Maybe.withDefault NotFound (Parser.parse route segments)


    -- toRoute "/blog/42"                            ==  NotFound
    -- toRoute "https://example.com/"                ==  Home
    -- toRoute "https://example.com/blog"            ==  NotFound
    -- toRoute "https://example.com/blog/42"         ==  Blog 42
    -- toRoute "https://example.com/blog/42/"        ==  Blog 42
    -- toRoute "https://example.com/blog/42#cats"    ==  Blog 42
    -- toRoute "https://example.com/blog/42?q=cats"  ==  Blog 42
    -- toRoute "https://example.com/settings"        ==  NotFound

Functions like `toRoute` are useful when creating single-page apps with
[`Browser.fullscreen`][fs]. I use them in `init` and `onNavigation` to handle
the initial URL and any changes.

[fs]: http://package.elm-lang.org/packages/elm-lang/browser/latest#fullscreen

-}
parse : Parser (a -> a) a -> Url -> Maybe a
parse (Parser parser) url =
    getFirstMatch <|
        parser <|
            State [] (preparePath url.path) (prepareQuery url.query) url.fragment identity


getFirstMatch : List (State a) -> Maybe a
getFirstMatch states =
    case states of
        [] ->
            Nothing

        state :: rest ->
            case state.unvisited of
                [] ->
                    Just state.value

                [ "" ] ->
                    Just state.value

                _ ->
                    getFirstMatch rest



-- PREPARE PATH


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        segment :: rest ->
            segment :: removeFinalEmpty rest



-- PREPARE QUERY


prepareQuery : Maybe String -> Dict String (List String)
prepareQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
    case String.split "=" segment of
        [ rawKey, rawValue ] ->
            case Http.decodeUri rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Http.decodeUri rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)



-- URL SEGMENTS


{-| The URL segments for webpages served over HTTP or HTTPS.

When you are creating a single-page app with [`Browser.fullscreen`][fs], you
use the [`parse`](#parse) function to turn a `Url` into nicely structured data.

[fs]: http://package.elm-lang.org/packages/elm-lang/browser/latest#fullscreen

**Note:** This is a subset of all the full possibilities listed in [the URI
spec](https://tools.ietf.org/html/rfc3986). Specifically, it does not accept
the `userinfo` segment you see in email addresses like `tom@example.com`.

-}
type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| Is the URL served over a secure connection or not?
-}
type Protocol
    = Http
    | Https


{-| Attempt to break a URL up into [`Url`](#Url). This is useful in
single-page apps when you want to parse certain chunks of a URL to figure out
what to show on screen.

    toUrl "https://example.com:443"
    -- Just
    --   { protocol = Https, host = "example.com", port = Just 443
    --   , path = "/", query = Nothing, fragment = Nothing
    --   }

    toUrl "https://example.com/hats?q=top"
    -- Just
    --   { protocol = Https, host = "example.com", port = Nothing
    --   , path = "/hats", query = Just "q=top", fragment = Nothing
    --   }

    toUrl "http://example.com/core/List/#map"
    -- Just
    --   { protocol = Http, host = "example.com", port = Nothing
    --   , path = "/core/List/", query = Nothing, fragment = Just "map"
    --   }

The conversion to segments can fail in some cases as well:

    toUrl "example.com:443"        == Nothing  -- no protocol
    toUrl "http://tom@example.com" == Nothing  -- userinfo disallowed
    toUrl "http://#cats"           == Nothing  -- no host

-}
toUrl : String -> Maybe Url
toUrl str =
    if String.startsWith "http://" str then
        chompAfterProtocol Http (String.dropLeft 7 str)
    else if String.startsWith "https://" str then
        chompAfterProtocol Https (String.dropLeft 8 str)
    else
        Nothing


chompAfterProtocol : Protocol -> String -> Maybe Url
chompAfterProtocol protocol str =
    if String.isEmpty str then
        Nothing
    else
        case String.indexes "#" str of
            [] ->
                chompBeforeFragment protocol Nothing str

            i :: _ ->
                chompBeforeFragment protocol (Just (String.dropLeft (i + 1) str)) (String.left i str)


chompBeforeFragment : Protocol -> Maybe String -> String -> Maybe Url
chompBeforeFragment protocol frag str =
    if String.isEmpty str then
        Nothing
    else
        case String.indexes "?" str of
            [] ->
                chompBeforeQuery protocol Nothing frag str

            i :: _ ->
                chompBeforeQuery protocol (Just (String.dropLeft (i + 1) str)) frag (String.left i str)


chompBeforeQuery : Protocol -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforeQuery protocol params frag str =
    if String.isEmpty str then
        Nothing
    else
        case String.indexes "/" str of
            [] ->
                chompBeforePath protocol "/" params frag str

            i :: _ ->
                chompBeforePath protocol (String.dropLeft i str) params frag (String.left i str)


chompBeforePath : Protocol -> String -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforePath protocol path params frag str =
    if String.isEmpty str || String.contains "@" str then
        Nothing
    else
        case String.indexes ":" str of
            [] ->
                Just <| Url protocol str Nothing path params frag

            i :: [] ->
                case Result.toMaybe (String.toInt (String.dropLeft (i + 1) str)) of
                    Nothing ->
                        Nothing

                    port_ ->
                        Just <| Url protocol (String.left i str) port_ path params frag

            _ ->
                Nothing


{-| Turn [`Url`](#Url) back into a `String`.
-}
fromUrl : Url -> String
fromUrl url =
    let
        http =
            case url.protocol of
                Http ->
                    "http://"

                Https ->
                    "https://"
    in
    addPort url.port_ (http ++ url.host)
        ++ url.path
        |> addPrefixed "?" url.query
        |> addPrefixed "#" url.fragment


addPort : Maybe Int -> String -> String
addPort maybePort starter =
    case maybePort of
        Nothing ->
            starter

        Just port_ ->
            starter ++ ":" ++ toString port_


addPrefixed : String -> Maybe String -> String -> String
addPrefixed prefix maybeSegment starter =
    case maybeSegment of
        Nothing ->
            starter

        Just segment ->
            starter ++ prefix ++ segment
