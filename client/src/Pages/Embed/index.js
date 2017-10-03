import initCodeMirror from '../../Views/Editors/CodeMirror'
import EditorsRunner from '../../Views/Editors/Runner'

initCodeMirror()
  .then(CodeMirror => {
    const Elm = require('./Main.elm')
    const app = Elm.Pages.Embed.Main.fullscreen()
    EditorsRunner.start(CodeMirror, app)
  })
