import 'es6-promise/auto'
import EllieUiIcon from '../../Ellie/Ui/Icon'
import EllieUiCodeEditor from '../../Ellie/Ui/CodeEditor'
import EllieUiOutput from '../../Ellie/Ui/Output'
import NetworkSocket from '../../Network/Socket'
import '../../Ellie/Ui/CodeEditor.css'
import { Pages } from "./Main.elm";

document.addEventListener('DOMContentLoaded', () => {
  let flags = {}
  const app = Pages.Embed.Main.fullscreen(flags)
  NetworkSocket.start(app)
  EllieUiOutput.start(app)
  EllieUiCodeEditor.start(app)
})
