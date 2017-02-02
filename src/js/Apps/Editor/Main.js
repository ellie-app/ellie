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
    var Elm = require('../../../elm/Apps/Editor/Main.elm')

    var app = Elm.Apps.Editor.Main.fullscreen({
      windowSize: {
        width: window.innerWidth,
        height: window.innerHeight
      },
      online: process.env.NODE_ENV === 'production' ? window.navigator.onLine : true
    })

    window.addEventListener('online', function () {
      app.ports.online.send(true)
    })

    window.addEventListener('offline', function () {
      app.ports.online.send(false)
    })

    window.addEventListener('beforeunload', function () {
      app.ports.windowUnloadedIn.send(null)
    })

    window.addEventListener('message', function (event) {
      app.ports.windowMessageIn.send(event.data)
    })
  })
