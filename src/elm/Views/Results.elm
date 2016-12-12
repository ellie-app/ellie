module Views.Results
    exposing
        ( loading
        )

import Html exposing (Html, div)
import Views.Classes exposing (Classes(..), class)


loadingSection : Html msg
loadingSection =
    div [ class [ LoadingResultsSection ] ]
        [ div [ class [ LoadingResultsFullBox ] ] []
        , div [ class [ LoadingResultsSplitContainer ] ]
            [ div [ class [ LoadingResultsSplitLeft ] ]
                [ div [ class [ LoadingResultsFullBox ] ] []
                , div [ class [ LoadingResultsFullBox ] ] []
                ]
            , div [ class [ LoadingResultsSplitRight ] ]
                [ div [ class [ LoadingResultsCircle ] ] []
                ]
            ]
        ]


loading : Html msg
loading =
    div [ class [ LoadingResults ] ]
        [ loadingSection
        , loadingSection
        , div [ class [ LoadingResultsFullBox ] ] []
        , div [ class [ LoadingShimmer ] ] []
        ]


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
