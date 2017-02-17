require('./Main.css')
require('../../../elm/Apps/Editor/Stylesheets.elm')
var register = require('./ServiceWorker')
var initCodeMirror = require('../../Shared/CodeMirror')

var promise =
  process.env.NODE_ENV === 'production' && navigator.serviceWorker ?
    register({ scope: '/' })
      .catch(function () {})
      .then(initCodeMirror) :
    initCodeMirror()

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
    var vimMode = window.location.search.indexOf('vim=true') !== -1

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

    window.addEventListener('unload', function (e) {
      console.dir(e)
      app.ports.windowUnloadedIn.send(null)
    })

    window.addEventListener('message', function (event) {
      app.ports.windowMessageIn.send(event.data)
    })

    if (vimMode) {
      require.ensure([
        'codemirror/keymap/vim',
        'codemirror/addon/dialog/dialog',
        'codemirror/addon/dialog/dialog.css'
      ], function () {
        require('codemirror/keymap/vim')
        require('codemirror/addon/dialog/dialog')
        require('codemirror/addon/dialog/dialog.css')
      })
    }
  })
