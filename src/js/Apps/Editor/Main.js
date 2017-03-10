if (!window.Promise) {
  require.ensure(['es6-promise'], function() {
    window.Promise = require('es6-promise')
    start()
  })
} else {
  start()
}

function start() {
  require('./Main.css')
  require('../../../elm/Apps/Editor/Stylesheets.elm')
  var register = require('./ServiceWorker')
  var initCodeMirror = require('../../Shared/CodeMirror')

  var vimMode = window.location.search.indexOf('vim=true') !== -1

  var promise =
    process.env.NODE_ENV === 'production' && navigator.serviceWorker ?
      register({ scope: '/' })
        .catch(function () {})
        .then(function () { return initCodeMirror(vimMode) }) :
      initCodeMirror(vimMode)

  promise
    .then(function () {
      var hasUnsavedWork = false;

      var previousLocation = window.location.pathname

      window.addEventListener('popstate', function (e) {
        if (hasUnsavedWork) {
          var result = window.confirm('You have unsaved work. Are you sure you want to go?')
          if (!result) {
            window.history.pushState({}, '', previousLocation)
            e.preventDefault()
          }
        }
      })

      var Elm = require('../../../elm/Apps/Editor/Main.elm')

      var app = Elm.Apps.Editor.Main.fullscreen({
        windowSize: {
          width: window.innerWidth,
          height: window.innerHeight
        },
        vimMode: vimMode,
        online: process.env.NODE_ENV === 'production' ? window.navigator.onLine : true
      })

      app.ports.pathChangedOut.subscribe(function () {
        previousLocation = window.location.pathname
      })

      app.ports.hasUnsavedWork.subscribe(function (nextValue) {
        hasUnsavedWork = nextValue
      })

      window.addEventListener('online', function () {
        app.ports.online.send(true)
      })

      window.addEventListener('offline', function () {
        app.ports.online.send(false)
      })

      window.addEventListener('beforeunload', function (e) {
        if (hasUnsavedWork) {
          e.returnValue = 'You have unsaved work. Are you sure you want to go?'
        }
      })

      // We need to do this out here because unfortunately Elm's ports are
      // asynchronous and by the time we get to the unload event we've only
      // got one tick left.
      window.addEventListener('unload', function (e) {
        var request = new XMLHttpRequest()
        request.withCredentials = true
        request.open('DELETE', API_BASE + '/session', false)
        request.send()
      })

      window.addEventListener('message', function (event) {
        if (event.origin !== API_BASE) return
        app.ports.windowMessageIn.send(event.data)
      })
    })
}
