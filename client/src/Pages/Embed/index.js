import initCodeMirror from '../../Ellie/CodeMirror/Loader'
import CodeMirrorRunner from '../../Ellie/CodeMirror/Runner'

initCodeMirror()
  .then(CodeMirror => {
    const Elm = require('./Main.elm')
    const app = Elm.Pages.Embed.Main.fullscreen()
    CodeMirrorRunner.start(CodeMirror, app)
  })
