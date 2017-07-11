import 'es6-promise/auto'
import './Stylesheets.elm'
import initCodeMirror from '../../Views/Editors/CodeMirror'

initCodeMirror()
  .then(() => {
    const Elm = require('./Main.elm')
    Elm.Pages.Embed.Main.fullscreen()
  })
