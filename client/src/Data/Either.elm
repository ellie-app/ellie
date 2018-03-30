module Data.Either
    exposing
        ( Either(..)
        , andMap
        , andThen
        , map
        , mapBoth
        , mapLeft
        , mapRight
        )


type Either l r
    = Left l
    | Right r


map : (a -> b) -> Either l a -> Either l b
map f either =
    case either of
        Left l ->
            Left l

        Right r ->
            Right (f r)


mapLeft : (a -> b) -> Either a r -> Either b r
mapLeft f either =
    case either of
        Left l ->
            Left (f l)

        Right r ->
            Right r


mapRight : (a -> b) -> Either l a -> Either l b
mapRight =
    map


mapBoth : (l -> ll) -> (r -> rr) -> Either l r -> Either ll rr
mapBoth fl fr either =
    case either of
        Left l ->
            Left (fl l)

        Right r ->
            Right (fr r)


andMap : Either l (a -> b) -> Either l a -> Either l b
andMap eitherf either =
    case ( eitherf, either ) of
        ( Right f, Right a ) ->
            Right (f a)

        ( Left l, _ ) ->
            Left l

        ( _, Left l ) ->
            Left l


andThen : (a -> Either l b) -> Either l a -> Either l b
andThen f either =
    case either of
        Right r ->
            f r

        Left l ->
            Left l
