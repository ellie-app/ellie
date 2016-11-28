module Editors
    exposing
        ( Language(..)
        , Editor
        , Editors
        , toList
        , update
        , init
        , getContent
        )

import Utils
import Tuple


type Language
    = Css
    | Html
    | Elm


type alias Editor =
    { isCollapsed : Bool
    , content : String
    , heightPercentage : Float
    }


type Editors
    = Editors (List ( Language, Editor ))


toList : Editors -> List ( Language, Editor )
toList (Editors editors) =
    editors


update : Language -> String -> Editors -> Editors
update language content (Editors editors) =
    editors
        |> List.map
            (\( lang, editor ) ->
                if lang == language then
                    ( lang, { editor | content = content } )
                else
                    ( lang, editor )
            )
        |> Editors


getContent : Language -> Editors -> String
getContent language (Editors editors) =
    editors
        |> Utils.listFind (\( lang, _ ) -> lang == language)
        |> Maybe.map (\( _, editor ) -> editor.content)
        |> Maybe.withDefault ""


init : Editors
init =
    Editors <|
        [ ( Html, Editor False initHtmlContent (1 / 3) )
        , ( Css, Editor False initCssContent (1 / 3) )
        , ( Elm, Editor False initElmContent (1 / 3) )
        ]
