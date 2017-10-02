import initCodeMirror from '../../Ellie/CodeMirror/Loader'
import CodeMirrorRunner from '../../Ellie/CodeMirror/Runner'
import IconLoader from '../../Ellie/Ui/Icon/Loader'

IconLoader.load()

initCodeMirror()
  .then(CodeMirror => {
    const Elm = require('./Main.elm')
    const app = Elm.Pages.Embed.Main.fullscreen()
    CodeMirrorRunner.start(CodeMirror, app)
  })
