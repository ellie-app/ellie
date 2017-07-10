import 'es6-promise/auto'
import '../../../elm/Apps/Embed/Stylesheets.elm'
import initCodeMirror from '../../Shared/CodeMirror'

initCodeMirror()
  .then(() => {
    const Elm = require('../../../elm/Apps/Embed/Main.elm')
    Elm.Pages.Embed.Main.fullscreen()
  })
