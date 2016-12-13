module Components.Output.View
    exposing
        ( loading
        , success
        , waiting
        , compiling
        )

import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (srcdoc)
import Components.Output.Classes exposing (Classes(..), class)
import Shared.Utils as Utils


loadingSection : Html msg
loadingSection =
    div [ class [ LoadingSection ] ]
        [ div [ class [ LoadingFullBox ] ] []
        , div [ class [ LoadingSplitContainer ] ]
            [ div [ class [ LoadingSplitLeft ] ]
                [ div [ class [ LoadingFullBox ] ] []
                , div [ class [ LoadingFullBox ] ] []
                ]
            , div [ class [ LoadingSplitRight ] ]
                [ div [ class [ LoadingCircle ] ] []
                ]
            ]
        ]


loading : Html msg
loading =
    div [ class [ Loading ] ]
        [ loadingSection
        , loadingSection
        , div [ class [ LoadingFullBox ] ] []
        , div [ class [ LoadingShimmer ] ] []
        ]


success : String -> String -> Html msg
success sessionId htmlCode =
    iframe
        [ srcdoc <| buildSrcDoc sessionId htmlCode
        , class [ Iframe ]
        ]
        []


overlayDisplay : String -> String -> Html msg
overlayDisplay title subtitle =
    div [ class [ Overlay ] ]
        [ div [ class [ OverlayTitle ] ]
            [ text title ]
        , div [ class [ OverlaySubtitle ] ]
            [ text subtitle ]
        ]


compiling : Html msg
compiling =
    overlayDisplay "Compiling!" "This shouldn't take too long."


waiting : Html msg
waiting =
    overlayDisplay "Ready!" "Run the compiler to see your program."


buildSrcDoc : String -> String -> String
buildSrcDoc sessionId htmlCode =
    Utils.stringReplace
        ("</head>")
        (resultScript sessionId ++ messagesScript ++ "</head>")
        (htmlCode)


resultScript : String -> String
resultScript sessionId =
    "<script src=\"http://localhost:1337/sessions/" ++ sessionId ++ "/result\"></script>"


messagesScript : String
messagesScript =
    """<script>
  document.addEventListener('mouseup', function () {
    parent.postMessage({ type: 'mouseup' }, 'http://localhost:8000')
  })
  document.addEventListener('mousemove', function (e) {
    parent.postMessage({
      type: 'mousemove',
      x: e.screenX,
      y: e.screenY
    }, 'http://localhost:8000')
  })
</script>"""
