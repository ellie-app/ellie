import CodeEditor from '../../Ellie/Ui/CodeEditor'
import IconLoader from '../../Ellie/Ui/Icon'

IconLoader.load()

CodeEditor
  .initialize()
  .then(() => {
    const Elm = require('./Main.elm')
    const app = Elm.Pages.Embed.Main.fullscreen()
  })
