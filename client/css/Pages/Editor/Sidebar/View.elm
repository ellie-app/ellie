module Pages.Editor.Sidebar.View exposing (..)

import Colors
import Css exposing (..)
import Css.Elements exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position relative
        , width (pct 100)
        , backgroundColor Colors.darkGray_
        ]


settings : UniqueClass
settings =
    uniqueClass
        [ position relative
        , padding2 zero (px 12)
        ]


setting : UniqueClass
setting =
    uniqueClass
        [ margin2 (px 16) zero ]


packagesSectionTitle =
    uniqueClass
        [ fontSize (px 14)
        , color Colors.lightGray_
        , paddingBottom (px 8)
        ]


packages =
    uniqueClass
        [ padding (px 12)
        ]


search =
    uniqueClass
        [ marginBottom (px 12) ]


package =
    uniqueClass
        [ marginBottom (px 12)
        , lastChild
            [ marginBottom zero ]
        ]


about =
    uniqueClass
        [ padding2 (px 16) (px 12)
        ]


aboutHeading =
    uniqueClass
        [ fontSize (px 16)
        , fontWeight bold
        , color Colors.lightGray_
        ]


aboutParagraph =
    uniqueClass
        [ fontSize (px 14)
        , color Colors.lightGray_
        , descendants
            [ a [ color Colors.pink_, textDecoration underline ]
            ]
        ]


aboutCopyright =
    uniqueClass
        [ fontSize (px 12)
        , color Colors.mediumGray_
        ]
