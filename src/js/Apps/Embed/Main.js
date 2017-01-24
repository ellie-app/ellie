// require('./Main.css')
require('../../../elm/Apps/Embed/Stylesheets.elm')
var register = require('./ServiceWorker')
var initCodeMirror = require('../../Shared/CodeMirror')

var promise =
  process.env.NODE_ENV === 'production' ?
    register({ scope: '/' })
      .catch(function () {})
      .then(initCodeMirror) :
    initCodeMirror()

promise
  .then(function () {
    var Elm = require('../../../elm/Apps/Embed/Main.elm')
    var app = Elm.Apps.Embed.Main.fullscreen()
  })
